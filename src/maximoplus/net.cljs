(ns maximoplus.net
  (:require
   [maximoplus.utils :as u]
   [maximoplus.net.browser :as b]
   )
  )

(declare serverRoot)
(declare get-tabsess)

(defn init [] (str ( serverRoot) "/server/init"))
(defn queuein [] (str ( serverRoot) "/server/queuein"))
(defn longpoll [] (str ( serverRoot) "/server/longpoll"))
(defn longpoll-batch [] (str ( serverRoot)  "/server/longpoll-batch"))
(defn register [] (str ( serverRoot)"/server/register"))
(defn login [] (str ( serverRoot) "/server/login"))
(defn logout[] (str (serverRoot) "/server/logout"))
(defn general-login [] (str (serverRoot) "/server/general-login"))
(defn command [] (str ( serverRoot) "/server/command"))
(defn sse [] (str (serverRoot) "/server/sse?t=" (get-tabsess)))

(defprotocol INet
  (-send-get
    [this url callback error-callback]
    [this url data callback error-callback])
  (-send-post
    [this url data callback error-callback]
    [this url data callback error-callback progress-callback])
  (-start-server-push-receiving
    [this callback error-callback])
  (-stop-server-push-receiving
    [this])
  (-get-tabsess;;tabsess handling will be done by the implemntation (browser or node)
    [this])
  (-set-tabsess!
    [this tabsess])
  );;abstract away get post and server push receiving for nodejs and browser

;;I decided there will be no nodejs implementation in the core lib, but by definng protocols, in the nodejs based libraries (GraphQL currently), it will be easier to define network functions


(deftype Browser []
  INet
  (-send-get
    [this url callback error-callback]
    (b/send-get url callback error-callback))
  (-send-get
    [this url callback error-callback data]
    (b/send-get-with-data url data callback error-callback))
  (-send-post
    [this url data callback error-callback]
    (b/send-post url data callback error-callback))
  (-send-post
    [this url data callback error-callback progress-callback]
    (b/send-post url data callback error-callback progress-callback))
  (-start-server-push-receiving
    [this callback error-callback]
    (b/start-server-push-receiving (sse) (longpoll-batch) false callback error-callback))
  (-stop-server-push-receiving
    [this]
    (b/stop-server-push-receiving))
  (-get-tabsess;;tabsess handling will be done by the implemntation (browser or node)
    [this]
    @b/tabsess)
  (-set-tabsess!
    [this tabsess]
    (reset! b/tabsess tabsess))
  )

(def net-type (atom (Browser.)))

(defn  set-net-type
  [type-inst]
  (reset! net-type type-inst))

(defn get-tabsess
  []
  (.log js/console "calling get-tabsess")
  (-get-tabsess @net-type))

(defn set-tabsess!
  [tabsess]
  (.log js/console "calling the set-tabsess " tabsess)
  (-set-tabsess! @net-type tabsess))

(defn send-get
  [url callback error-callback & data]
  (if data
    (-send-get @net-type url callback error-callback (first data))
    (-send-get @net-type url callback error-callback)))

(defn send-post
  [url data callback error-callback & progress-callback]
  (if progress-callback
    (-send-post @net-type url data callback error-callback (first progress-callback))
    (-send-post @net-type url data callback error-callback)))

(defn start-server-push-receiving
  [callback error-callback]
  (-start-server-push-receiving @net-type callback error-callback))

(defn stop-server-push-receiving
  []
  (-stop-server-push-receiving @net-type))

;;so i don't have to include the nodejs implemtation (and the headache about compiling and packing the required nodejs en twork libraries), this is left for the dependand clojurescipt libraries (GraphQL nodejs , and React Native in the future). 

(def ^:export globalFunctions
  #js {"serverRoot" (fn [] "")
       "longpollTimeout"  1800000 ;half an hour like default session
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

(defn ^:export upload [container method params data callback errback progress-callback]
  (send-post (getUploadURL container method params true) data callback errback progress-callback))

(defn ^:export set-server-root
  [url]
  (aset globalFunctions "serverRoot" (fn [_] url)))
