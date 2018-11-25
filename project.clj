(defproject dirc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.312"]
                 [cljsjs/nacl-fast "1.0.0-rc.1-0"]
                 [mvxcvi/alphabase "0.2.2"]
                 [binaryage/oops "0.6.2"]
                 [reagent "0.8.1"]]

  :plugins [[lein-cljsbuild "1.1.5"]
            [lein-figwheel "0.5.16"]]

  :min-lein-version "2.5.0"
  :clean-targets ^{:protect false}
  [:target-path
   [:cljsbuild :builds :app :compiler :output-dir]
   [:cljsbuild :builds :app :compiler :output-to]]

  :resource-paths ["public"]

  :figwheel {:http-server-root "."
             :nrepl-port 7002
             :nrepl-middleware ["cemerick.piggieback/wrap-cljs-repl"]
             :css-dirs ["public/css"]}

  :cljsbuild {:builds {:app
                       {:source-paths ["src" "env/dev/cljs"]
                        :compiler
                        {:main "dirc.dev"
                         :output-to "public/js/app.js"
                         :output-dir "public/js/out"
                         :asset-path   "js/out"
                         :install-deps true
                         :npm-deps {"bencode-js" "0.0.8"
                                    "webtorrent" "0.100.0"
                                    "bugout" "0.0.8"}
                         :foreign-libs [{:file "node_modules/bugout/docs/bugout.min.js"
                                         :provides ["cljsjs.bugout"]
                                         :global-exports {cljsjs.bugout Bugout}}
                                        {:file "node_modules/webtorrent/webtorrent.min.js"
                                         :provides ["cljsjs.webtorrent"]
                                         :global-exports {cljsjs.webtorrent WebTorrent}}]
                         :source-map true
                         :optimizations :none
                         :pretty-print  true}
                        :figwheel
                        {:on-jsload "dirc.core/mount-root"}}
                       :release
                       {:source-paths ["src" "env/prod/cljs"]
                        :compiler
                        {:output-to "build/js/app.js"
                         :output-dir "public/js/release"
                         :asset-path   "js/out"
                         :install-deps true
                         :pseudo-names true
                         ;:source-map "build/js/app.js.map"
                         :npm-deps {"bencode-js" "0.0.8"
                                    "webtorrent" "0.100.0"
                                    "bugout" "0.0.8"}
                         :foreign-libs [{:file "node_modules/bugout/docs/bugout.min.js"
                                         :provides ["cljsjs.bugout"]
                                         :global-exports {cljsjs.bugout Bugout}}
                                        {:file "node_modules/webtorrent/webtorrent.min.js"
                                         :provides ["cljsjs.webtorrent"]
                                         :global-exports {cljsjs.webtorrent WebTorrent}}]
                         :optimizations :advanced
                         :pretty-print false}}}}

  :aliases {"package" ["do" "clean" ["cljsbuild" "once" "release"]]}

  :profiles {:dev {:source-paths ["src" "env/dev/clj"]
                   :dependencies [[binaryage/devtools "0.9.7"]
                                  [figwheel-sidecar "0.5.16"]
                                  [org.clojure/tools.nrepl "0.2.13"]
                                  [com.cemerick/piggieback "0.2.2"]]}})
