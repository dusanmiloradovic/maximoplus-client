(ns maximoplus.net
  (:require
   [maximoplus.utils :as u]
   [maximoplus.net.browser :as b :refer [Browser]]
   [maximoplus.net.rn :refer [ReactNative]]
   [cljs.core.async :refer [put! <! >! chan buffer poll!]]
   [maximoplus.net.protocols :refer [-send-get -send-post -start-server-push-receiving -stop-server-push-receiving -get-tabsess -set-tabsess!]]
   )
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  )

(declare serverRoot)
(declare get-tabsess)

(defn init [] (str ( serverRoot) "/server/init"))
(defn queuein [] (str ( serverRoot) "/server/queuein"))
(defn register [] (str ( serverRoot)"/server/register"))
(defn login [] (str ( serverRoot) "/server/login"))
(defn logout[] (str (serverRoot) "/server/logout"))
(defn general-login [] (str (serverRoot) "/server/general-login"))
(defn command [] (str ( serverRoot) "/server/command"))
(defn sse [] (str (serverRoot) "/server/sse?t=" (get-tabsess)))

(def net-type
  (if (and
       js/navigator
       (= (aget js/navigator "product") "ReactNative" ))
    (do
      (println "Setting net type to React Native")
      (atom (ReactNative.)))
    (do
      (println "Setting net type to browser")
      (atom (Browser.)))))

;;(def command-pipeline (chan (buffer 1)))

;;(go-loop []
;;  (let [{url :url callback :callback error-callback :errback data :data} (<! command-pipeline)]
;;    (if data
;;      (-send-get @net-type url callback error-callback (first data))
;;      (-send-get @net-type url callback error-callback))
;;    (recur)))

;;in ndoejs we are getting the stackoverflow wxception, because of the huge chaing of callbacks. If I pipe the send-get calls to the async channel I may fix this.
;;Next step - instead of the global promise for the commands on the containers, use the channel. Only return the immediate promise for the user

(defn  set-net-type
  [type-inst]
  (reset! net-type type-inst))

(defn get-tabsess
  []
  (-get-tabsess @net-type))

(defn set-tabsess!
  [tabsess]
  (-set-tabsess! @net-type tabsess))

(defn send-get
  [url callback error-callback & data]
  (if data
    (-send-get @net-type url callback error-callback (first data))
    (-send-get @net-type url callback error-callback)))

;;(defn send-get
;;  [url callback error-callback & data]
;;  (let [o {:url url :callback callback :errback error-callback :data (when data (first data))}]
;;    (put! command-pipeline o)))

(defn send-post
  [url data callback error-callback & progress-callback]
  (if progress-callback
    (-send-post @net-type url data callback error-callback (first progress-callback))
    (-send-post @net-type url data callback error-callback)))

(defn start-server-push-receiving
  [callback error-callback]
  (-start-server-push-receiving @net-type (sse) callback error-callback))

(defn stop-server-push-receiving
  []
  (-stop-server-push-receiving @net-type))

;;so i don't have to include the nodejs implemtation (and the headache about compiling and packing the required nodejs en twork libraries), this is left for the dependand clojurescipt libraries (GraphQL nodejs , and React Native in the future). 

(def ^:export globalFunctions
  #js {"serverRoot" (fn [] "")
       }
  )

(defn ^:export  serverRoot []
  (.call (aget globalFunctions "serverRoot")))

(defn ^:export virtualSessionString []
  (str "t=" (get-tabsess)))

(defn cb [x]
  (if x (u/debug (str "callback" x))))

(defn errcb [x]
  (u/debug "error callback"))

(defn param-list [params]
  (reduce (fn[x y] (str x "&" y)) (map (fn[[k v]] (str k "=" v )) params)))

(defn ^:export getUploadURL [container method params & skip-tabsess]
  (let [p (merge
           {"method" method "control-name" (.getId container)}
           (when-not skip-tabsess {"t" (get-tabsess)}) 
           (when params (-> params js->clj)))]
    (str (serverRoot) "/server/upload?" (param-list p))))

(defn ^:export getDownloadURL [container method params & skip-tabsess]
  (let [p (merge
           {"method" method "control-name" (.getId container) }
           (when-not skip-tabsess {"t" (get-tabsess)}) 
           (when params (-> params  js->clj)))]
    (str (serverRoot) "/server/binary-content?" (param-list p))))

;;we can assume this function will be called from the javascript only, so we need to trasnform the errback payload to javascript
(defn ^:export upload [container method params data callback errback progress-callback]
  (send-post (getUploadURL container method params true) data
             callback
             (fn [[[mx err err-group err-code] _] _ _]
               (errback err))  progress-callback))

(defn ^:export setServerRoot
  [url]
  (aset globalFunctions "serverRoot" (fn [_] url)))
