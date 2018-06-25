(ns ^:figwheel-no-load dirc.dev
  (:require
    [dirc.core :as core]
    [devtools.core :as devtools]))


(enable-console-print!)

(devtools/install!)

(core/init!)
