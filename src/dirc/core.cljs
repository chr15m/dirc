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

(defn string-to-uint8array [s]
  (if (= (type s) js/Uint8Array)
    s
    (.encode utf8encoder s)))

(defn fingerprint [x]
  (-> x
      (to-hex)
      (.substring 0 8)))

;; Crypto

(defn hash-object [t]
  (-> t
      (clj->js)
      (bencode/encode)
      (js/Uint8Array.from)
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
  (nacl.randomBytes 8))

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

(defn receive-message [wire message]
  (let [decoded-js (bencode/decode (.toString message))
        decoded (js->clj decoded-js)]
    (debug "message:" decoded)))

(defn handle-handshake [wire addr handshake]
  (debug "handle-handshake" (.. wire -peerId) addr handshake))

(defn wire-fn [pk wire]
  (set! (.. wire -extendedHandshake -pk) pk))

(defn attach-extension-protocol [wire addr]
  (let [t (partial wire-fn "PUBLIC KEY")]
    (set! (.. t -prototype -name) BT-EXT)
    (set! (.. t -prototype -onExtendedHandshake) (partial handle-handshake wire addr))
    (set! (.. t -prototype -onMessage) (partial receive-message wire))
    t))

(defn detach-wire [wire]
  (debug "closed" (.-peerId wire)))

(defn attach-wire [wire addr]
  (debug "saw wire" (.-peerId wire))
  (.use wire (attach-extension-protocol wire addr))
  (.on wire "close" (partial detach-wire wire)))

(defn joined-channel [state torrent]
  (debug "joined-channel" (.-infoHash torrent))
  (swap! state assoc-in [:channels (.-infoHash torrent) :state] :connected))

(defn join-channel [state buffer]
  (debug "join-channel" buffer)
  (let [channel-hash (to-hex (.slice (hash-object buffer) 0 20))
        channel-file (js/File. [channel-hash] channel-hash)]
    (debug "channel-hash" channel-hash)
    (swap! state assoc-in [:channels channel-hash :state] :connecting)
    (let [torrent (.seed (@state :wt) channel-file #js {:name channel-hash} (partial joined-channel state))]
      (debug "torrent" torrent)
      (.on torrent "infoHash" #(debug "channel-hash verify:" %))
      (.on torrent "wire" (partial attach-wire)))))

(defn send-message [state buffer]
  (debug "send-message" buffer))

;; -------------------------
;; Event handlers

(defn handle-submit [state buffer ev]
  (.preventDefault ev)
  (let [tokens (.split @buffer " ")
        first-word (first tokens)]
    (cond (= first-word "/join") (join-channel state (second tokens))
          :else (send-message state @buffer))
    (reset! buffer "")))

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
     (for [[h c] (get @state :channels)]
       [:span.tab {:key (str h)}
        [component-icon :times-circle]
        (c :name)])]
    [:div#messages "Messages here"]
    [component-input-box state]]])

;; -------------------------
;; Initialize app

(defonce state
  (r/atom {:wt (WebTorrent.)}))

(defn mount-root []
  (r/render [home-page state] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
