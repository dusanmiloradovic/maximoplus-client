(ns maximoplus.net
  (:require
   [goog.net.NetworkStatusMonitor :as nstat]
   [goog.net.ErrorCode :as ec]
   [goog.net.EventType :as gevt]
   [maximoplus.utils :as u]
   [goog.events]
   [goog.events.OnlineHandler]
   [maximoplus.arrays :as ar]
   )
  (:import [goog.net XhrIo])
)

;;this is more or less copy/paste from clojure/browser/net,aimed at reducing the  file size for advanced compilation

(defprotocol IConnection
  (connect
    [this]
    [this opt1]
    [this opt1 opt2]
    [this opt1 opt2 opt3])
  (transmit
    [this opt]
    [this opt opt2]
    [this opt opt2 opt3]
    [this opt opt2 opt3 opt4]
    [this opt opt2 opt3 opt4 opt5])
  (close [this]))

(def *timeout* 10000)

(extend-type XhrIo
  IConnection
  (transmit
    ([this uri]
       (transmit this uri "GET"  nil nil *timeout*))
    ([this uri method]
       (transmit this uri method nil nil *timeout*))
    ([this uri method content]
       (transmit this uri method content nil *timeout*))
    ([this uri method content headers]
       (transmit this uri method content headers *timeout*))
    ([this uri method content headers timeout]
       (.setTimeoutInterval this timeout)
     (.send this uri method content headers))
    ))

(defn xhr-connection
  "Returns an XhrIo connection"
  []
  (XhrIo.))

(def ^:export globalFunctions
  #js {"serverRoot" (fn [] "")
       "longpollTimeout"  1800000 ;half an hour like default session
       }
  )

(defn ^:export  serverRoot []
  (.call (aget globalFunctions "serverRoot")))

(def tabsess (atom "123"))

(defn init [] (str ( serverRoot) "/server/init"))
(defn queuein [] (str ( serverRoot) "/server/queuein"))
(defn longpoll [] (str ( serverRoot) "/server/longpoll"))
(defn longpoll-batch [] (str ( serverRoot)  "/server/longpoll-batch"))
(defn register [] (str ( serverRoot)"/server/register"))
(defn login [] (str ( serverRoot) "/server/login"))
(defn logout[] (str (serverRoot) "/server/logout"))
(defn general-login [] (str (serverRoot) "/server/general-login"))
(defn command [] (str ( serverRoot) "/server/command"))
(defn sse [] (str (serverRoot) "/server/sse?t=" @tabsess))

(defn translate-goog-xhr-code
  [code]
  ;;https://google.github.io/closure-library/source/closure/goog/net/errorcode.js
  (case code
    0 "NO_ERROR"
    1 "ACCESS_DENIED"
    2 "FILE_NOT_FOUND"
    3 "FF_SILENT_ERROR"
    4 "CUSTOM_ERROR"
    5 "EXCEPTION"
    6 "HTTP_ERROR"
    7 "ABORT"
    8 "TIMEOUT"
    9 "OFFLINE")
  )

(defn get-response-vector
  [xhr1]
  (let [status (. xhr1 (getStatus))]
    (if (= "218" status)
      status
      (let [resp-text(.getResponseText xhr1) ]
        [(try (u/transit-read resp-text)
              (catch js/Error e
                (u/debug e)
                nil))
         (translate-goog-xhr-code (.getLastErrorCode xhr1))
         status]
        ))))

(defn callback-from-xhr
  [xhr1 callback error-callback]
  (u/debug "zovem callback from xhr " (. xhr1 (getLastErrorCode)))
  (if (= ec/NO_ERROR (. xhr1 (getLastErrorCode)))
    (callback (get-response-vector xhr1  ))
    (error-callback (get-response-vector xhr1))))
  
(defn send-meth [cf url callback error-callback & progress-callback]
  (let [xhr1 (xhr-connection)
        _ap  (if (= -1 (.indexOf url "?")) "?" "&")
        _url (str url _ap "t=" @tabsess)
        pc (if progress-callback (first progress-callback))]
    (u/debug "Zovem send-meth " url)
    (.setWithCredentials xhr1 true)
    (goog.events/listenOnce xhr1
                       "complete"
                       (fn[e]
                        ; (u/debug "xhr call for  " url " finished at " (.getTime (js/Date.)))
                         (callback-from-xhr xhr1 callback error-callback)
                         (.dispose xhr1)
                         ))
    (when pc
      (goog.events/listen xhr1 gevt/UPLOAD_PROGRESS (fn [ev] (pc (aget ev "loaded") (aget ev "total")))) )
    (cf xhr1 _url)))

(defn send-get [url callback error-callback]
 ; (u/debug "calling send-get for  " url " at " (.getTime (js/Date.)))
  (try 
    (send-meth (fn [xhr1 url] (transmit xhr1 url "GET" nil nil 0)) url callback error-callback)
    (catch js/Error e (u/debug "Error in AJAX call:" e))))

(defn send-get-with-data [url data callback error-callback]
  (send-meth (fn [xhr1 url] (transmit xhr1 url "GET" data nil 0)) url callback error-callback))

(defn send-post [url data callback error-callback & progress-callback]
  (send-meth (fn [xhr1 url] (transmit xhr1 url "POST" data nil 0)) url callback error-callback
             (first progress-callback)))

;(defn send [url callback-map]
;  (let [xhr1 (xhr-connection)]
;    (.setWithCredentials xhr1 true)
;    (doseq [method (keys callback-map)]
;      (event/listen xhr1 method
;                    (fn[e] ((method callback-map) e))))
;    (transmit xhr1 url)))

(defn ^:export virtualSessionString []
  (str "t=" @tabsess))

(defn cb [x]
  (if x (u/debug (str "callback" x)))
  )

(defn errcb [x]
  (u/debug "error callback")
  )

(def *run-the-longpoll* (atom false))
;this is set to false for easier start-up control from maximoplus.core package
;if it was true that would indicate the longpoll is already running, so it couldn't be started

(def *longpoll-min* 5000)

(defn long-poll
  [url callback error-callback]
  (when @*run-the-longpoll*
    (let [kk (xhr-connection)
          start-time (.now js/Date)]
      (.setWithCredentials kk true)
      (.setTimeoutInterval kk 10000)
      (goog.events/listenOnce kk
                              gevt/SUCCESS
                             (fn [e]
                               (let [resp-text (if ( = "" (.getResponseText kk)) "" (u/transit-read (.getResponseText kk)))
                                     status (. kk (getStatus))]
                                 (callback resp-text)
                                 (long-poll url callback error-callback))))
      (goog.events/listenOnce kk
                              gevt/TIMEOUT
                              (fn [e]
                                (long-poll url callback error-callback)))
      (goog.events/listenOnce kk
                              gevt/ERROR
                              (fn [e]
                                        ;seems like internet explorer doesn't handle correctly the timeouts, i.e. it does not send the timeout event, when it timeouts, instead it will give the error. The temporary workaround is to check the time when the request has started, and if it is long enough (more than 5 secs), assume that the long poll timed out and do it again
                                (let [ek (.getLastErrorCode kk)
                                      err (.getLastError kk)
                                      status (.getStatus kk)
                                      curr-ts (.now js/Date)]
                                  (if (> (- curr-ts start-time) *longpoll-min*)
                                    (long-poll url callback error-callback)
                                    (do
                                      (u/debug "long poll error")
                                      (error-callback  [ek err status]))))))
      (transmit kk (str url "?t=" @tabsess)))))


(def event-source (atom nil))

(defn sse-start
  [callback error-callback]
  (let [ev-s (js/EventSource. (sse) #js {:withCredentials true})]
    (reset! event-source ev-s)
    (.addEventListener ev-s "message"
                       (fn [message]
                         (let [_data (aget message "data")
                               data (if (= "" _data) "" (u/transit-read _data))]
                           (u/debug "SSE" data)
                           (callback data))))
    (.addEventListener ev-s "error"
                       (fn [error]
                         (error-callback error)))))

(defn sse-stop
  []
  (when @event-source
    (.close @event-source)
    (reset! event-source nil)))

(defn listen-offline [cb]
  (let [oh (goog.events.OnlineHandler.)]
    (when-not (.isOnline oh)
      (cb true));ensure that the offline flag will be set to 1 immediately, so the controls don't depend on the event
    (goog.events/listen oh (js/Array. (.-ONLINE nstat/EventType) (.-OFFLINE nstat/EventType))
                        (fn []
                          (cb (not (.isOnline oh)))))))

(defn param-list [params]
  (reduce (fn[x y] (str x "&" y)) (map (fn[[k v]] (str k "=" v )) params)))

(defn ^:export getUploadURL [container method params & skip-tabsess]
  (let [p (merge
           {"method" method "control-name" (.getId container)}
           (when-not skip-tabsess {"t" @tabsess}) 
           (when params (-> params js->clj)))]
    (str (serverRoot) "/server/upload?" (param-list p))))

(defn ^:export getDownloadURL [container method params & skip-tabsess]
  (let [p (merge
           {"method" method "control-name" (.getId container) }
           (when-not skip-tabsess {"t" @tabsess}) 
           (when params (-> params  js->clj)))]
    (str (serverRoot) "/server/binary-content?" (param-list p))))

(defn ^:export upload [container method params data callback errback progress-callback]
  (send-post (getUploadURL container method params true) data callback errback progress-callback))

(defn ^:export set-server-root
  [url]
  (aset globalFunctions "serverRoot" (fn [_] url)))
