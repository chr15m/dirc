(ns dirc.core
  (:require
    [reagent.core :as r]
    [cljsjs.bugout :as Bugout]
    [cljsjs.webtorrent :as WebTorrent]
    ["bencode-js/lib/index" :as bencode]
    [cljsjs.nacl-fast :as nacl]
    [oops.core :refer [ocall!]]))

;; -------------------------
;; Constants

(def help-message
"/help
/join #CHANNEL-NAME = join a channel by name
/nick handle = set your visible handle
/key = print your hex encoded public key
/logout = completely delete keys

https://github.com/chr15m/dirc/#self-hosted-install")

; TODO:
; * command history
; * button to turn URLs into inline content
; * /seed [hex-encoded-seed] = get or set your keypair seed
; * refactor out most of the crypto fns below into a lib - mostly unused

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

(defn string-to-uint8array [s]
  (if (= (type s) js/Uint8Array)
    s
    (ocall! utf8encoder "encode" s)))

(defonce utf8decoder (js/TextDecoder. "utf8"))

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

(defn restore-map-list [saved-vec]
  (into {} (map (fn [[k v]] [(name k) (into '() (reverse v))]) saved-vec)))

(defn restore-map [saved-vec]
  (into {} (map (fn [[k v]] [(name k) v]) saved-vec)))

(defn extract-account [a]
  {:seed (aget a "seed")
   :users (restore-map (js->clj (aget a "users") :keywordize-keys true))
   :messages (restore-map-list (js->clj (aget a "messages") :keywordize-keys true))})

(defn load-account []
  (let [account-js (storage-load "dirc")
        account (if account-js
                  (extract-account account-js)
                  (create-account))]
    (js/console.log "account-js" account-js)
    account))

(defn save-account [state]
  (let [saving {:seed (state :seed)
                :users (-> state :users)
                :messages (-> state :messages)}]
    (js/console.log "saving:" (clj->js saving))
    (storage-save "dirc" saving)))

;; Network

(defn get-profile [state]
  (-> @state :users (get (@state :address))))

(defn get-channel-bugout [state channel-hash]
  (-> state :channels (get channel-hash) :bugout))

(defn make-message [kind content]
  (clj->js
    (merge {:k kind
            :n (to-hex (random-bytes 16))}
           content)))

(defn send-message [state channel-hash kind & [content to-address]]
  (let [b (get-channel-bugout @state channel-hash)
        packet (make-message kind content)]
    (if to-address
      (.send b to-address packet)
      (.send b packet))
    state))

(defn send-profile [state channel-hash & [to-address]]
  (send-message state channel-hash :profile {:p (get-profile state)} to-address))

(defn send-profile-update [state]
  (doseq [[channel-hash channel] (@state :channels)]
    (send-profile state channel-hash)))

(defn append-message [state channel-hash message]
  (update-in state [:messages channel-hash] conj message))

(defn network-event-receive-message [state channel-hash address message & [packet]]
  (debug "message" address message)
  (let [m (js->clj message :keywordize-keys true)
        result (case (m :k)
                 "hello" (send-profile state channel-hash address)
                 "message" (swap! state append-message channel-hash (merge m {:pk address :t (now)}))
                 "profile" (swap! state assoc-in [:users address] (m :p))
                 (do (js/console.error "unknown message kind:" (clj->js m)) false))]
    (save-account @state)
    result))

(defn send-message-if-connected [state channel-hash kind & [content to-address]]
  (when (> (-> @state :channels (get channel-hash) :connections) 0)
    (send-message state channel-hash kind content to-address)))

(defn network-event-user-state-update [state channel-hash op msg address]
  (swap! state #(-> %
                    (update-in [:channels channel-hash :users] op address)
                    (append-message channel-hash {:m msg :n (to-hex (random-bytes 16)) :pk address :t (now)}))))

(defn network-event-join [state channel-hash address]
  (network-event-user-state-update state channel-hash conj "joined" address)
  (send-profile-update state)
  (send-message-if-connected state channel-hash :hello {} address))

(defn get-selected-channel [state]
  (get-in state [:ui :selected]))

(defn is-selected-channel? [state channel-hash]
  (= (get-selected-channel state) channel-hash))

;; -------------------------
;; Actions

(defn join-channel [state channel-name]
  (let [channel-hash (.substr (to-hex (hash-object {:channel channel-name})) 0 16)]
    (when (not (get-in @state [:channels channel-hash]))
      (let [bugout (Bugout. (str "dir-channel-" channel-name) #js {:wt (@state :wt) :seed (ocall! Bugout "encodeseed" (@state :seed)) :timeout 60000})]
        (debug "join-channel" channel-name channel-hash)
        (swap! state
               #(-> %
                    (assoc-in [:channels channel-hash] {:connections nil :name channel-name :bugout bugout :users #{}})
                    (assoc-in [:ui :selected] channel-hash)))
        ; hook up the Bugout events
        (debug "bugout address" (.address bugout))
        (ocall! bugout "on" "seen" (partial #'network-event-join state channel-hash))
        (ocall! bugout "on" "left" (partial #'network-event-user-state-update state channel-hash disj "left"))
        (ocall! bugout "on" "message" (partial #'network-event-receive-message state channel-hash))
        (ocall! bugout "on" "connections" #(swap! state assoc-in [:channels channel-hash :connections] %))
        (ocall! bugout "heartbeat")
        (network-event-user-state-update state channel-hash conj "joined" (.address bugout))
        @state))))

(defn leave-channel [state channel-hash]
  (ocall! (get-channel-bugout @state channel-hash) "destroy" (partial debug "Bugout instance destroyed."))
  (swap! state
         #(-> %
              (update-in [:channels] dissoc channel-hash)
              (update-in [:ui :selected] (fn [selected] (if (= selected channel-hash) "log" selected))))))

(defn add-log-message [state c & message-parts]
  (swap! state update-in [:log] conj
         {:m (apply str message-parts)
          :c c
          :t (now)}))

(defn update-nick [state nick]
  (swap! state assoc-in [:users (@state :address) :handle] nick)
  (save-account @state)
  (send-profile-update state)
  @state)

(defn send-chat-message [state buffer]
  (let [channel-hash (get-selected-channel @state)
        kind :message
        message {:m @buffer :c channel-hash}
        sent (send-message-if-connected state channel-hash kind message)
        ; send to self if not to others
        result (or sent (network-event-receive-message state channel-hash (@state :address) (make-message kind message)))]
    (save-account @state)
    result))

(defn select-channel [state channel-hash & [ev]]
  (swap! state assoc-in [:ui :selected] channel-hash)
  (.focus (js/document.getElementById "chatbox")))

;; -------------------------
;; UI Fns & Event handlers

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
                                (not (is-selected-channel? @state "log"))) (send-chat-message state buffer)
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
        [:button {:type "submit" :id "send"} [component-icon "comment"]]]])))

(defn component-messages [state]
  [(with-meta
     (fn []
       [:div#messages
        (if (is-selected-channel? @state "log")
          (doall (for [m (reverse (get-in @state [:log]))]
                   [:div {:key (m :t) :class (m :c)}
                    [:span.time {:title (get-date (m :t))} (get-time (m :t))]
                    [:pre.message (m :m)]]))
          (doall (for [m (reverse (get-in @state [:messages (get-selected-channel @state)]))]
                   (when (m :m)
                     [:div {:key (str (m :t) (m :pk) (m :n))}
                      [:span.time {:title (get-date (m :t))} (get-time (m :t))]
                      [:span.who {:title (m :pk)} (or (get-in @state [:users (m :pk) :handle]) "?")]
                      [:pre.message (m :m)]]))))])
     {:component-will-update check-and-scroll-to-bottom})])

(defn component-channel-list [state]
  [:div#channels
   [:div.tab {:key "log"
              :class (when (is-selected-channel? @state "log") "selected")
              :on-click (partial select-channel state "log")}
    [:span "-"]
    [:span.name "log"]]
   (doall (for [[h c] (@state :channels)]
            [:div.tab {:key (str h)
                       :class (when (is-selected-channel? @state h) "selected")}
             [:span {:on-click (partial leave-channel state h)}
              [component-icon :times-circle]]
             [:span {:on-click (partial select-channel state h)}
              [:span.connections
               (if (= (count (c :users)) 0)
                 ".."
                 (c :connections))]
              " "
              [:span.name (c :name)]]]))])

(defn component-user-list [state]
  [:div#users
   (doall (for [u (get-in @state [:channels (get-selected-channel @state) :users])]
            [:div {:key u}
             [:span.handle (-> @state :users (get u) :handle)]
             [:span.id u]]))])

(defn component-burger-menu [state]
  (when (@state :burger)
    [:div#channel-info
     (or (apply + (map (fn [[channel-hash channel]] (channel :connections)) (@state :channels))) 0)
     " wires"
     [:hr]
     [component-channel-list state]
     [:hr]
     [component-user-list state]]))

(defn home-page [state]
  (if @state
    [:div#wrapper
     [:div#message-area
      [component-messages state]
      [component-input-box state]]
     [component-burger-menu state]
     [:button#burger {:on-click #(swap! state update-in [:burger] not)}
      [component-icon "bars"]]]
    [:div#fin "fin."]))

;; -------------------------
;; Initialize app

(defonce state
  (let [saved-state (load-account)
        seed (saved-state :seed)
        keypair (keypair-from-seed seed)
        address (ocall! Bugout "address" (aget keypair "publicKey"))]
    (r/atom
      {:wt (WebTorrent. (storage-load "dirc-wt-config"))
       :b []
       :seed seed
       :keypair (keypair-from-seed seed)
       :address address
       :users (saved-state :users)
       :messages (saved-state :messages)
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
  (add-watch state :logger #(debug %4))
  (mount-root))
