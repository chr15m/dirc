(ns dirc.core
  (:require
    [reagent.core :as r]
    [cljsjs.webtorrent :as WebTorrent]
    ["bencode-js/lib/index" :as bencode]
    [cljsjs.nacl-fast :as nacl]))

;; -------------------------
;; Functions

(defn to-hex [b]
  (.join (.map (js/Array.from (.slice b)) #(.slice (str "0" (.toString (bit-and % 0xff) 16)) -2)) ""))

(defn from-hex [b]
  (js/Uint8Array. (map #(js/parseInt (apply str %) 16) (partition 2 b))))

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

;; -------------------------
;; Event handlers

(defn handle-submit [ev]
  (.preventDefault ev)
  (js/console.log ev))

;; -------------------------
;; Views

(defn component-input-box []
  (let [buffer (r/atom "")]
    (fn []
      [:div#input
       [:form {:on-submit (partial handle-submit)}
        [:input {:value @buffer
                 :placeholder "..."
                 :on-change #(reset! buffer (-> % .-target .-value))}]
        [:button {:type "submit" :id "send"} "send"]]])))

(defn home-page []
  [:div#wrapper
   [:div#channel-info "Channel info"]
   [:div#message-area
    [:div#channels
     [:span.tab "One"]
     [:span.tab.selected "Two"]
     [:span.tab "Three"]]
    [:div#messages "Messages here"]
    [component-input-box]]])

;; -------------------------
;; Initialize app

(defonce state
  (r/atom {:wt (WebTorrent.)}))

(defn mount-root []
  (r/render [home-page state] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
