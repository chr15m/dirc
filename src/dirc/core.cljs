(ns dirc.core
  (:require
    [reagent.core :as r]
    [cljsjs.bugout :as Bugout]
    [cljsjs.webtorrent :as WebTorrent]
    ["bencode-js/lib/index" :as bencode]
    [cljsjs.nacl-fast :as nacl]
    [oops.core :refer [oget ocall! oset!]]))

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

(defn get-channel-bugout [state channel-hash]
  (-> state :channels (get channel-hash) :bugout))

(defn append-message [state channel-hash message]
  (update-in state [:channels channel-hash :messages] conj message))

(defn network-event-receive-message [state channel-hash address message packet]
  (debug "message" address message)
  (let [m (js->clj message :keywordize-keys true)]
    (case (m :k)
      "message" (swap! state append-message channel-hash (merge m {:pk address :t (now)}))
      "profile" (swap! state assoc-in [:users address] (m :p)))))

(defn network-event-user-state-update [state channel-hash op msg address]
  (swap! state #(-> %
                    (update-in [:channels channel-hash :users] op address)
                    (append-message channel-hash {:m msg :n (to-hex (random-bytes 16)) :pk address :t (now)}))))

(defn send-message [state channel-hash kind content]
  (if (> (-> @state :channels (get channel-hash) :connections) 0)
    (do
      (.send (get-channel-bugout @state channel-hash)
             (clj->js (merge {:k kind
                              :n (to-hex (random-bytes 16))}
                             content)))
      state)))

(defn send-profile-update [state]
  (doseq [[channel-hash channel] (@state :channels)]
    (send-message state channel-hash :profile {:p (@state :profile)})))

(defn join-channel [state channel-name]
  (let [channel-hash (.substr (to-hex (hash-object {:channel channel-name})) 0 16)]
    (when (not (get-in @state [:channels channel-hash]))
      (let [bugout (Bugout. (str "dir-channel-" channel-name) #js {:wt (@state :wt) :seed (ocall! Bugout "encodeseed" (@state :seed)) :timeout 60000})]
        (debug "join-channel" channel-name channel-hash)
        (swap! state #(-> % (assoc-in [:channels channel-hash] {:connections nil :name channel-name :bugout bugout :users #{}})
                          (assoc-in [:ui :selected] channel-hash)))
        ; hook up the Bugout events
        (debug "bugout address" (.address bugout))
        (ocall! bugout "on" "seen" #((network-event-user-state-update state channel-hash conj "joined" %) (send-profile-update state)))
        (ocall! bugout "on" "left" (partial #'network-event-user-state-update state channel-hash disj "left"))
        (ocall! bugout "on" "message" (partial #'network-event-receive-message state channel-hash))
        (ocall! bugout "on" "connections" #(swap! state assoc-in [:channels channel-hash :connections] %))
        (ocall! bugout "heartbeat")
        @state))))

(defn leave-channel [state channel-hash]
  ; destroy the bugout instance
  (.destroy (get-channel-bugout @state channel-hash) (partial debug "Bugout instance destroyed."))
  (swap! state update-in [:channels] dissoc channel-hash))

(defn add-log-message [state c & message-parts]
  (swap! state update-in [:log] conj
         {:m (apply str message-parts)
          :c c
          :t (now)}))

(defn update-nick [state nick]
  (swap! state assoc-in [:profile :handle] nick)
  (send-profile-update state)
  @state)

;; -------------------------
;; UI Fns & Event handlers

(defn select-channel [state channel-hash & [ev]]
  (swap! state assoc-in [:ui :selected] channel-hash)
  (.focus (js/document.getElementById "chatbox")))

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
                                (not (is-selected-channel? @state "log"))) (let [channel-hash (get-selected-channel @state)] (send-message state channel-hash :message {:m @buffer :c channel-hash}))
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
          (if (> (count (get-in @state [:channels (get-selected-channel @state) :users])) 0)
            (doall (for [m (reverse (get-in @state [:channels (get-selected-channel @state) :messages]))]
                     (when (m :m)
                       [:div {:key (str (m :t) (m :pk) (m :n))}
                        [:span.time {:title (get-date (m :t))} (get-time (m :t))]
                        [:span.who {:title (m :pk)} (or (get-in @state [:users (m :pk) :handle]) "?")]
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
                 (if (= (count (c :users)) 0)
                   ".."
                   (c :connections))
                 " "
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
      {:wt (WebTorrent. (storage-load "dirc-wt-config"))
       :b []
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
  (add-watch state :logger #(debug %4))
  (mount-root))
