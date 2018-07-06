(ns dirc.core
  (:require
    [reagent.core :as r]
    [cljsjs.webtorrent :as WebTorrent]
    ["bencode-js/lib/index" :as bencode]
    [cljsjs.nacl-fast :as nacl]))

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

(defonce utf8encoder (js/TextEncoder. "utf8"))
(defonce utf8decoder (js/TextDecoder. "utf8"))

(defn string-to-uint8array [s]
  (if (= (type s) js/Uint8Array)
    s
    (.encode utf8encoder s)))

(defn uint8array-to-string [a]
  (if (= (type a) js/String)
    a
    (.decode utf8decoder a)))

(defn now []
  (-> (js/Date.) (.getTime)))

;; Crypto

(defn fingerprint [x]
  (-> x
      (string-to-uint8array)
      (nacl.hash)
      (to-hex)
      (.substring 0 8)))

(defn hash-object [t]
  (-> t
      (clj->js)
      (bencode/encode)
      (string-to-uint8array)
      (nacl.hash)))

(defn make-keypair []
  (nacl.sign.keyPair))

(defn sign-datastructure [keypair datastructure]
  (nacl.sign.detached
    (hash-object datastructure)
    (aget keypair "secretKey")))

(defn check-datastructure-signature [pk datastructure]
  (let [signature (datastructure :signature)
        datastructure (dissoc datastructure :signature)]
    (nacl.sign.detached.verify
      (hash-object datastructure)
      signature
      pk)))

(defn make-nonce []
  (nacl.randomBytes 16))

;; Storage

(defonce storage (aget js/window "localStorage"))

(defn serializer [k v]
  (cond
    (= (type v) js/Uint8Array) (str "0x" (to-hex v))
    :else v))

(defn deserializer [k v]
  (cond
    (and (= (type v) js/String) (.substr v 0 2)) (from-hex (.substr v 2))
    :else v))

(defn storage-load [k]
  (-> storage (.getItem k) (js/JSON.parse deserializer) (js->clj :keywordize-keys true)))

(defn storage-save [k v]
  (.setItem storage k (-> v clj->js (js/JSON.stringify serializer))))

;; Network

(def BT-EXT "dc_channel")

(defn receive-message [state wire message]
  (debug "receive-message")
  (debug "receive-message wire" wire)
  (debug "receive-message message" message)
  (let [channel-hash (.. wire -extendedHandshake -channelhash)
        public-key (.. wire -extendedHandshake -pk)
        message-string (uint8array-to-string message)
        decoded-js (bencode/decode message-string)
        decoded (js->clj decoded-js)
        decoded (assoc decoded "pk" public-key)]
    (print "receive-message decoded:" decoded)
    (swap! state update-in [:channels channel-hash :messages] conj decoded)))

(defn handle-handshake [wire addr handshake]
  (debug "handle-handshake" (.. wire -peerId) addr handshake))

(defn wire-fn [pk channel-hash wire]
  (set! (.. wire -extendedHandshake -pk) pk)
  (set! (.. wire -extendedHandshake -channelhash) channel-hash))

(defn attach-extension-protocol [state channel-hash wire addr]
  (let [t (partial wire-fn "PUBLIC KEY" channel-hash)]
    (set! (.. t -prototype -name) BT-EXT)
    (set! (.. t -prototype -onExtendedHandshake) (partial handle-handshake wire addr))
    (set! (.. t -prototype -onMessage) (partial receive-message state wire))
    t))

(defn detach-wire [wire]
  (debug "closed" (.-peerId wire)))

(defn attach-wire [state channel-hash wire addr]
  (debug "saw wire" (.-peerId wire))
  (.use wire (attach-extension-protocol state channel-hash wire addr))
  (.on wire "close" (partial detach-wire wire)))

(defn joined-channel [state channel-hash torrent]
  (let [info-hash (.-infoHash torrent)
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
    (let [torrent (.seed (@state :wt) channel-blob #js {:name channel-hash} (partial joined-channel state channel-hash))]
      (swap! state #(-> % (assoc-in [:channels channel-hash] {:state :connecting :name buffer})
                        (assoc-in [:ui :selected] channel-hash)))
      (debug "channel torrent" torrent)
      (.on torrent "infoHash" #(debug "channel-hash verify:" %))
      (.on torrent "wire" (partial attach-wire state channel-hash)))))

(defn remove-channel [state channel-hash]
  (swap! state update-in [:channels] dissoc channel-hash))

(defn leave-channel [state channel-hash]
  (let [info-hash (get-in @state [:channels channel-hash :info-hash])]
    (if info-hash
      (.remove (@state :wt) info-hash
               (fn []
                 (remove-channel state channel-hash)))
      (remove-channel state channel-hash))))

(defn send-message [state buffer]
  (let [payload {:message buffer
                 :timestamp (now)
                 :nonce (to-hex (random-bytes 16))}
        payload (clj->js payload)]
    (debug (aget (@state :wt) "torrents"))
    (doall
      (for [t (aget (@state :wt) "torrents")]
        (do
          (debug "torrent" t)
          (doseq [w (.-wires t)]
            (do
              (debug "wire" w)
              (.extended w BT-EXT payload))))))
    (debug "send-message" payload)))

(defn get-selected-channel [state]
  (get-in state [:ui :selected]))

(defn is-selected-channel? [state channel-hash]
  (= (get-selected-channel state) channel-hash))

;; -------------------------
;; Event handlers

(defn handle-submit [state buffer ev]
  (.preventDefault ev)
  (let [tokens (.split @buffer " ")
        first-word (first tokens)]
    (cond (= first-word "/join") (join-channel state (second tokens))
          :else (send-message state @buffer))
    (reset! buffer "")))

(defn select-channel [state channel-hash ev]
  (swap! state assoc-in [:ui :selected] channel-hash))

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
        [:input {:auto-focus true
                 :value @buffer
                 :placeholder "..."
                 :on-change #(reset! buffer (-> % .-target .-value))}]
        [:button {:type "submit" :id "send"} [:img.icon {:src "icons/comment.svg"}]]]])))

(defn home-page [state]
  [:div#wrapper
   [:div#channel-info
    [:div#buttons
     [component-icon :bars]
     [component-icon :cog]]
    "Users"]
   [:div#message-area
    [:div#channels
     (doall (for [[h c] (get @state :channels)]
              [:span.tab {:key (str h)
                          :class (when (is-selected-channel? @state h) "selected")
                          :on-click (partial select-channel state h)}
               [:span {:on-click (partial leave-channel state h)}
                [component-icon :times-circle]]
               (when (= (c :state) :connecting)
                 ".. ")
               (c :name)]))]
    [:div#messages
     (doall (for [m (reverse (get-in @state [:channels (get-selected-channel @state) :messages]))]
              [:div {:key (str (m "timestamp") (m "pk") (m "nonce"))}
               [:span.time (m "timestamp")]
               [:span.who (m "pk")]
               [:span.message (m "message")]]))]
    [component-input-box state]]])

;; -------------------------
;; Initialize app

(defonce state
  (r/atom {:wt (WebTorrent.)}))

(defn mount-root []
  (debug "WebTorrent:" (@state :wt))
  (debug "State:" @state)
  (r/render [home-page state] (.getElementById js/document "app")))

(defn init! []
  (.on (@state :wt) "torrent"
       (fn [torrent]
         (debug "WebTorrent torrent:")))
  (mount-root))
