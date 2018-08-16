(ns dirc.core
  (:require
    [reagent.core :as r]
    [cljsjs.webtorrent :as WebTorrent]
    ;["webtorrent/webtorrent.min" :as WebTorrent]
    ["bencode-js/lib/index" :as bencode]
    [cljsjs.nacl-fast :as nacl]
    [oops.core :refer [ocall! oset!]]))

;; -------------------------
;; Constants

(defonce utf8encoder (js/TextEncoder. "utf8"))
(defonce utf8decoder (js/TextDecoder. "utf8"))

(def help-message
"/help
/join #CHANNEL-NAME = join a channel by name
/nick handle = set your visible handle
/key = print your hex encoded public key
/logout = completely delete keys

https://github.com/chr15m/dirc/#self-hosted-install")

; /seed [hex-encoded-seed] = get or set your keypair seed

;; -------------------------
;; Functions

(if (aget js/localStorage "debug")
  (defn debug [& args]
    (apply js/console.log (clj->js args)))
  (def debug identity))

(defn to-hex [b]
  (.join (.map (js/Array.from (.slice b)) #(.slice (str "0" (.toString (bit-and % 0xff) 16)) -2)) ""))

(defn from-hex [b]
  (js/Uint8Array. (map #(js/parseInt (apply str %) 16) (partition 2 b))))

(defn string-to-uint8array [s]
  (if (= (type s) js/Uint8Array)
    s
    (ocall! utf8encoder "encode" s)))

(defn uint8array-to-string [a]
  (if (= (type a) js/String)
    a
    (ocall! utf8decoder "decode" a)))

(defn now []
  (-> (js/Date.) (.getTime)))

(defn zero-pad [x]
  (.substr (str "00" x) -2))

(defn get-date [t]
  (let [d (js/Date. t)]
    (str (.getFullYear d)
         "-"
         (zero-pad (.getMonth d))
         "-"
         (zero-pad (.getDay d)))))

(defn get-time [t]
  (let [d (js/Date. t)]
    (str (zero-pad (.getHours d))
         ":"
         (zero-pad (.getMinutes d)))))

;; Crypto

(defn fingerprint [x]
  (let [h (-> x
              (string-to-uint8array)
              (nacl.hash)
              (to-hex)
              (.substring 0 16))]
    (->> h
         (partition 4)
         (map clojure.string/join)
         (clojure.string/join " "))))

(defn hash-object [t]
  (-> t
      (clj->js)
      (bencode/encode)
      (string-to-uint8array)
      (nacl.hash)))

(defn sign-datastructure [keypair datastructure]
  (assoc datastructure
         :sig (to-hex
                (nacl.sign.detached
                  (hash-object datastructure)
                  (aget keypair "secretKey")))))

(defn check-datastructure-signature [pk datastructure]
  (let [signature (from-hex (datastructure :sig))
        datastructure (dissoc datastructure :sig)]
    (nacl.sign.detached.verify
      (hash-object datastructure)
      signature
      pk)))

(defn keypair-from-seed [seed]
  (nacl.sign.keyPair.fromSeed seed))

(defn random-bytes [length]
  (nacl.randomBytes length))

;; Storage

(defonce storage (aget js/window "localStorage"))

(defn serializer [k v]
  (cond
    (= (type v) js/Uint8Array) (str "0x" (to-hex v))
    :else v))

(defn deserializer [k v]
  (cond
    (and (= (type v) js/String) (= (.substr v 0 2) "0x")) (from-hex (.substr v 2))
    :else v))

(defn storage-load [k]
  (-> storage (.getItem k) (js/JSON.parse deserializer)))

(defn storage-save [k v]
  (.setItem storage k (-> v clj->js (js/JSON.stringify serializer))))

(defn storage-remove [k]
  (.removeItem storage k))

(defn create-account []
  {:seed (random-bytes 32)
   :profile {:handle "anonymous"}})

(defn extract-account [a]
  {:seed (aget a "seed")
   :profile (js->clj (aget a "profile") :keywordize-keys)})

(defn load-account []
  (let [account-js (storage-load "dirc")
        account (if account-js
                  (extract-account account-js)
                  (create-account))]
    (js/console.log "account-js" account-js)
    account))

(defn save-account [state]
  (storage-save "dirc" {:seed (state :seed)
                        :profile (state :profile)}))

;; Network

(def BT-EXT "dc_channel")

(defn get-channel-infohash [state channel-hash]
  (get-in state [:channels channel-hash :info-hash]))

(defn broadcast-message-to-channel [state channel-hash packet]
  (let [wt (state :wt)
        torrent (ocall! wt "get" (get-channel-infohash state channel-hash))
        wires (aget torrent "wires")]
    ; send to all wires
    (debug "sending to" (count wires) "wires")
    (doseq [w wires]
      (ocall! w "extended" BT-EXT packet))
    state))

(defn receive-message [state wire message]
  (debug "receive-message" wire message)
  (let [channel-hash (aget wire "extendedHandshake" "channelhash")
        message-string (uint8array-to-string message)
        decoded-js (bencode/decode message-string)
        payload (js->clj decoded-js :keywordize-keys true)]
    (debug "channel-hash" channel-hash)
    (print "receive-message payload:" payload)
    (if (= channel-hash (payload :c))
      (if (check-datastructure-signature (from-hex (payload :pk)) payload)
        (swap! state (fn [old-state]
                       ; TODO: check timestamp or send a sequence number to prevent replay attacks
                       ; update this user's details
                       (let [new-state (assoc-in old-state [:users (payload :pk)] (payload :u))
                             ; update the last-seen time from this user
                             new-state (assoc-in new-state [:users (payload :pk) :last] (payload :t))
                             ; add the message if it is a message
                             new-state (if (not (contains? (set (get-in new-state [:channels channel-hash :messages])) payload))
                                         (do
                                           ; re-send if not received already
                                           (broadcast-message-to-channel new-state channel-hash decoded-js)
                                           (update-in new-state [:channels channel-hash :messages] conj payload))
                                         new-state)]
                         new-state)))
        (debug "dropped message with bad signature:" decoded-js))
      (debug "dropped message with wrong channel-hash:" decoded-js))))

(defn handle-handshake [wire addr handshake]
  (debug "handle-handshake" (aget wire "peerId") addr handshake)
  (debug "extension:" (aget wire "extendedHandshake")))

(defn wire-fn [channel-hash wire]
  (aset (aget wire "extendedHandshake") "channelhash" channel-hash))

(defn attach-extension-protocol [state channel-hash wire addr]
  (let [t (partial wire-fn channel-hash)]
    (aset (aget t "prototype") "name" BT-EXT)
    (aset (aget t "prototype") "onExtendedHandshake" (partial #'handle-handshake wire addr))
    (aset (aget t "prototype") "onMessage" (partial #'receive-message state wire))
    (debug "extension:" t)
    t))

(defn detach-wire [state channel-hash wire]
  (debug "closed" (aget wire "peerId"))
  (swap! state update-in [:channels channel-hash :wires] dissoc (aget wire "peerId")))

(defn attach-wire [state channel-hash wire addr]
  (debug "saw wire" (aget wire "peerId"))
  (swap! state assoc-in [:channels channel-hash :wires (aget wire "peerId")] true)
  (ocall! wire "use" (attach-extension-protocol state channel-hash wire addr))
  (ocall! wire "on" "close" (partial detach-wire state channel-hash wire)))

(defn joined-channel [state channel-hash torrent]
  (let [info-hash (aget torrent "infoHash")
        chan-cursor (r/cursor state [:channels channel-hash])]
    (debug "joined-channel" info-hash)
    (swap! chan-cursor assoc :state :connected :info-hash info-hash)))

(defn make-channel-blob [channel-hash]
  (js/File. [channel-hash] channel-hash))

(defn join-channel [state buffer]
  (debug "join-channel" buffer)
  (let [channel-hash (to-hex (.slice (hash-object {:channel-name buffer}) 0 20))
        channel-blob (make-channel-blob channel-hash)]
    (debug "channel-hash" channel-hash)
    (let [torrent (ocall! (@state :wt) "seed" channel-blob #js {:name channel-hash} (partial joined-channel state channel-hash))]
      (swap! state #(-> % (assoc-in [:channels channel-hash] {:state :connecting :name buffer})
                        (assoc-in [:ui :selected] channel-hash)))
      (debug "channel torrent" torrent)
      (ocall! torrent "on" "infoHash" #(debug "channel info-hash verify:" %))
      (ocall! torrent "on" "wire" (partial attach-wire state channel-hash))
      @state)))

(defn remove-channel [state channel-hash]
  (swap! state update-in [:channels] dissoc channel-hash))

(defn leave-channel [state channel-hash]
  (let [info-hash (get-channel-infohash @state channel-hash)]
    (if info-hash
      (ocall! (@state :wt) "remove" info-hash
               (fn []
                 (remove-channel state channel-hash)))
      (remove-channel state channel-hash))))

(defn make-packet [keypair profile channel-hash payload]
  (let [packet {:c channel-hash
                :t (now)
                :n (to-hex (random-bytes 16))
                :pk (to-hex (aget keypair "publicKey"))
                :u profile}
        packet (merge packet payload)
        packet (sign-datastructure keypair packet)
        packet (clj->js packet)]
    packet))

(defn send-message [state buffer channel-hash]
  (let [packet (make-packet (state :keypair) (state :profile) channel-hash {:m buffer})]
    (debug "send-message" packet)
    (broadcast-message-to-channel state channel-hash packet))
  state)

(defn send-ping [state]
  (when @state
    (doseq [[channel-hash c] (@state :channels)]
      (let [packet (make-packet (@state :keypair) (@state :profile) channel-hash {:p 1})]
        (broadcast-message-to-channel @state channel-hash packet)))
    @state))

(defn update-nick [state nick]
  (swap! state assoc-in [:profile :handle] nick)
  (send-ping state)
  @state)

;; -------------------------
;; UI Fns & Event handlers

(defn select-channel [state channel-hash & [ev]]
  (swap! state assoc-in [:ui :selected] channel-hash)
  (.focus (js/document.getElementById "chatbox")))

(defn add-log-message [state c & message-parts]
  (swap! state update-in [:log] conj
         {:m (apply str message-parts)
          :c c
          :t (now)}))

(defn get-selected-channel [state]
  (get-in state [:ui :selected]))

(defn is-selected-channel? [state channel-hash]
  (= (get-selected-channel state) channel-hash))

(defn handle-submit [state buffer ev]
  (.preventDefault ev)
  (let [tokens (.split @buffer " ")
        first-word (first tokens)
        action-taken (cond (= first-word "/join") (if (= (first (second tokens)) "#") (join-channel state (second tokens)) (do (add-log-message state :error "Channel name must start with '#'.") false))
                           (= first-word "/help") (add-log-message state :info help-message)
                           (= first-word "/nick") (update-nick state (second tokens))
                           (= first-word "/key") (add-log-message state :info (str "Your public key: " (to-hex (aget (@state :keypair) "publicKey"))))
                           (= first-word "/logout") (do (storage-remove "dirc") (reset! state nil))
                           (= (first first-word) "/") (do (add-log-message state :error "No such command: " @buffer) false)
                           (and (> (count @buffer) 0)
                                (not (is-selected-channel? @state "log"))) (send-message @state @buffer (get-selected-channel @state))
                           :else false)]
    (when action-taken
      (reset! buffer ""))))

(defn check-and-scroll-to-bottom [element]
  (when element
    (let [el (aget js/document "documentElement")]
      (let [is-stuck (<= (- (.-scrollHeight el) (.-clientHeight el)) (+ (.-scrollTop el) 1))]
        (if is-stuck
          ; hack to scroll to the bottom after update
          (js/setTimeout
            #(aset el "scrollTop" (.-scrollHeight el))
            1))))))

;; -------------------------
;; Views

(defn component-icon [n & [selected]]
  [:img.icon {:src (str "icons/" (name n) ".svg")
              :class (if selected "selected" "")}])

(defn component-input-box [state]
  (let [buffer (r/atom "")]
    (fn []
      [:div#input
       [:form {:on-submit (partial handle-submit state buffer)}
        [:input {:id "chatbox"
                 :auto-focus true
                 :value @buffer
                 :placeholder "..."
                 :on-change #(reset! buffer (-> % .-target .-value))}]
        [:button {:type "submit" :id "send"} [:img.icon {:src "icons/comment.svg"}]]]])))

(defn component-messages [state]
  [(with-meta
     (fn []
       [:div#messages
        (if (is-selected-channel? @state "log")
          (doall (for [m (reverse (get-in @state [:log]))]
                   [:div {:key (m :t) :class (m :c)}
                    [:span.time {:title (get-date (m :t))} (get-time (m :t))]
                    [:pre.message (m :m)]]))
          (if (> (-> (get-in @state [:channels (get-selected-channel @state)]) :wires count) 0)
            (doall (for [m (reverse (get-in @state [:channels (get-selected-channel @state) :messages]))]
                     (when (m :m)
                       [:div {:key (str (m :t) (m :pk) (m :n))}
                        [:span.time {:title (get-date (m :t))} (get-time (m :t))]
                        [:span.who {:title (fingerprint (m :pk))} (or (get-in @state [:users (m :pk) :handle]) "?")]
                        [:pre.message (m :m)]])))
            [:div.waiting "Waiting for other participants" [:span.dot-1 "."] [:span.dot-2 "."] [:span.dot-3 "."]]))])
     {:component-will-update check-and-scroll-to-bottom})])

(defn home-page [state]
  (if @state
    [:div#wrapper
     [:div#channel-info
      (apply + (map (fn [[channel-hash channel]] (-> channel :wires count)) (@state :channels)))
      " wires"]
     [:div#message-area
      [:div#channels
       [:span.tab {:key "log"
                   :class (when (is-selected-channel? @state "log") "selected")
                   :on-click (partial select-channel state "log")}
        "log"]
       (doall (for [[h c] (@state :channels)]
                [:span.tab {:key (str h)
                            :class (when (is-selected-channel? @state h) "selected")
                            :on-click (partial select-channel state h)}

                 [:span {:on-click (partial leave-channel state h)}
                  [component-icon :times-circle]]
                 (when (= (c :state) :connecting)
                   ".. ")
                 (c :name)]))]
      [component-messages state]
      [component-input-box state]]]
    [:div#fin "fin."]))

;; -------------------------
;; Initialize app

(defonce state
  (let [saved-state (load-account)
        seed (saved-state :seed)]
    (r/atom
      ; var client = new WebTorrent({ tracker: { rtcConfig: {}, wrtc: {} } })
      {:wt (WebTorrent. (storage-load "dirc-wt-config"))
       :seed seed
       :profile (saved-state :profile)
       :keypair (keypair-from-seed seed)
       :users {}
       :ui {:selected "log"}
       :log ()})))

(defn mount-root []
  (when @state
    (debug "WebTorrent:" (@state :wt))
    (debug "State:" @state)
    (save-account @state)
    (add-log-message state :info help-message)
    (let [url-hash (aget js/window "document" "location" "hash")]
      (when (not= url-hash "")
        (join-channel state url-hash))))
  (r/render [home-page state] (.getElementById js/document "app")))

(defn init! []
  (ocall! (@state :wt) "on" "torrent"
       (fn [torrent]
         (debug "WebTorrent torrent:" (aget torrent "infoHash"))))
  ; periodically send out a ping to everyone
  (js/setInterval
    (partial #'send-ping state)
    30000)
  (mount-root))
