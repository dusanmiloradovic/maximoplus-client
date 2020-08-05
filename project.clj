(defproject maximoplus-client "1.0.0-SNAPSHOT"
  :description "core maximoplus client - clojurescript library"
  :dependencies [
                 [org.clojure/clojurescript  "1.10.773"]
                 [com.cognitect/transit-cljs "0.8.256"]
                 [weasel "0.7.0"]
                 [cider/piggieback "0.3.6"]
                 [org.clojure/clojure "1.10.0"]
                 [org.clojure/core.async "0.3.443"]
                 [thheller/shadow-cljs "2.8.40"]
                 ]


  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-localrepo "0.5.4"]
            [cider/cider-nrepl "0.17.0"]
	    ]
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
                   :optimizations :advanced
                   :foreign-libs [{:file "js-foreign/out/es.js"
                                   :provides ["rn-eventsource"]}]
                   :closure-warnings {:externs-validation :off
                                      :non-standard-jsdoc :off}
                   }}]}
               }]
             :dev
             [{  :cljsbuild
               {:builds
                [
                 {:source-paths ["src"],
                  :compiler
                  {:pretty-print false
                   ;;      :output-wrapper "(function(){%s};).call(window);"
                   :output-wrapper true,
                   :output-dir "public/javascript/maximoplus-core",
                   :output-to "public/javascript/maximoplus-core/main.js",
                   :closure-output-charset "US-ASCII"
                   :optimizations :advanced
                   :foreign-libs [{:file "js-foreign/out/es.js"
                                   :provides ["rn-eventsource"]}]
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
                   ;;                   :language-in :ecmascript5
                   ;;                 :language-out :ecmascript5-strict
                   :foreign-libs [{:file "js-foreign/out/es.js"
                                   :provides ["rn-eventsource"]}]
                   :hashbang false
                   :optimizations :simple
                   :verbose true
                   :closure-warnings {:externs-validation :off
                                      :non-standard-jsdoc :off}}}


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
		   :foreign-libs [{:file "js-foreign/out/es.js"
                                   :provides ["rn-eventsource"]}]
                   :externs ["js-foreign/externs.js"]
                   :hashbang false
                   :optimizations :advanced
                   :closure-warnings {:externs-validation :off
                                      :non-standard-jsdoc :off}
                   }}


                 ]}}]
             :rnbundle
             [{  :cljsbuild
               {:builds
                [
                 {:source-paths ["src" "src-repl"],
                  :compiler
                  {:pretty-print false,
                   :output-to "public/javascript/react-native-prod/main.js",
                   :output-dir "public/javascript/react-native-prod",
                   :target :bundle
                   ;;                   :parallel-build true
                   :closure-output-charset "US-ASCII"
		   :foreign-libs [{:file "js-foreign/out/es.js"
                                   :provides ["rn-eventsource"]}]
                   :externs ["js-foreign/externs.js"]
                   :hashbang false
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
