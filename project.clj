(defproject maximoplus-client "1.0.0-SNAPSHOT"
  :description "core maximoplus client - clojurescript library"
  :dependencies [
;;                 [org.clojure/clojurescript  "1.10.339"]
;;                 [com.cognitect/transit-cljs "0.8.256"]
;;                 [weasel "0.7.0"]
  ;;               [cider/piggieback "0.3.6"]
                 [org.clojure/clojure "1.10.0"]
;;                 [org.clojure/core.async "0.3.443"]
                 [thheller/shadow-cljs "2.8.40"]
                 ]


;;  :plugins [[lein-cljsbuild "1.1.7"]
;;            [lein-localrepo "0.5.4"]
;;            [cider/cider-nrepl "0.17.0"]
;;	    ]
  :profiles {:publish;;no source map, important, because webpack fails if it doesn't find the source map from the end of the file
             [{:cljsbuild
               {:builds
                [{:source-paths ["src"],
                  :compiler
                  {:pretty-print false
                   ;;      :output-wrapper "(function(){%s};).call(window);"
                   :output-wrapper true,
                   :output-dir "public/javascript/maximoplus-core",
                   :output-to "public/javascript/maximoplus-core/main.js",
                   :closure-output-charset "US-ASCII"
                   :language-in :ecmascript5
                   :language-out :ecmascript5
                   :parallel-build true
                   :optimizations :advanced
                   :closure-warnings {:externs-validation :off
                                      :non-standard-jsdoc :off}
                   }}]}
               }]
             :dev
             [{  :cljsbuild
               {:builds
                [
                 ;;                 {:source-paths ["src" "src-repl"],
                 ;;                  :compiler
                 ;;                  {:pretty-print true,
                 ;;                   :output-to "public/javascript/main.js",
                 ;;                   :output-dir "public/javascript",
                 ;;                   :closure-output-charset "US-ASCII"
                 ;;                   :language-in :ecmascript5
                 ;;                   :language-out :ecmascript5-strict
                 ;;                   :optimizations :whitespace
                 ;;                   :closure-warnings {:externs-validation :off
                 ;;                                      :non-standard-jsdoc :off}
                 ;;                   }}
                 ;;    {:source-paths ["src" "src-repl"],
                 ;;     :compiler
                 ;;     {:pretty-print true,
                 ;;      :output-to "public/javascript/simple/main.js",
                 ;;      :output-dir "public/javascript/simple",
                 ;;      :closure-output-charset "US-ASCII"
                 ;;      :optimizations :simple
                 ;;      :language-in :ecmascript5
                 ;;      :language-out :ecmascript5-strict
                 ;;      :closure-warnings {:externs-validation :off
                 ;;                         :non-standard-jsdoc :off}
                 ;;      }}
                 {:source-paths ["src"],
                  :compiler
                  {:pretty-print false
                   ;;      :output-wrapper "(function(){%s};).call(window);"
                   :output-wrapper true,
                   :output-dir "public/javascript/maximoplus-core",
                   :output-to "public/javascript/maximoplus-core/main.js",
                   :closure-output-charset "US-ASCII"
                   :language-in :ecmascript5
                   :language-out :ecmascript5
                   :optimizations :advanced
                   :parallel-build true
                   :source-map "public/javascript/maximoplus-core/source-map.js.map"
                   :closure-warnings {:externs-validation :off
                                      :non-standard-jsdoc :off}
                   }}
                 ]}}]
             :rn
             [{  :cljsbuild
               {:builds
                [
                 {:source-paths ["src" "src-repl"],
                  :compiler
                  {:pretty-print true,
                   :output-to "public/javascript/react-native/main.js",
                   :output-dir "public/javascript/react-native",
                   :target :nodejs
                   ;;                   :parallel-build true
                   :closure-output-charset "US-ASCII"
                   :language-in :ecmascript5
                   :language-out :ecmascript5-strict
                   :optimizations :simple
                   :closure-warnings {:externs-validation :off
                                      :non-standard-jsdoc :off}
                   }}


                 ]}}]
             :rnprod
             [{  :cljsbuild
               {:builds
                [
                 {:source-paths ["src" "src-repl"],
                  :compiler
                  {:pretty-print false,
                   :output-to "public/javascript/react-native-prod/main.js",
                   :output-dir "public/javascript/react-native-prod",
                   :target :nodejs
                   ;;                   :parallel-build true
                   :closure-output-charset "US-ASCII"
                   :language-in :ecmascript5
                   :language-out :ecmascript5-strict
                   :optimizations :advanced
                   :closure-warnings {:externs-validation :off
                                      :non-standard-jsdoc :off}
                   }}


                 ]}}]
             :whitespace
             [{  :cljsbuild
               {:builds
                [
                 {:source-paths ["src" "src-repl"],
                  :compiler
                  {:pretty-print true,
                   :output-dir "public/javascript/maximoplus-core",
                   :output-to "public/javascript/maximoplus-core/main.js",
                   ;;                   :parallel-build true
                   :closure-output-charset "US-ASCII"
                   :language-in :ecmascript5
                   :language-out :ecmascript5-strict
                   :optimizations :whitespace
                   :closure-warnings {:externs-validation :off
                                      :non-standard-jsdoc :off}
                   }}


                 ]}}]
             }
  ;;  :jvm-opts ["-Xms12g" "-Xmx16g"]
  )

(require 'cemerick.pomegranate.aether)
(cemerick.pomegranate.aether/register-wagon-factory!
 "http" #(org.apache.maven.wagon.providers.http.HttpWagon.))
