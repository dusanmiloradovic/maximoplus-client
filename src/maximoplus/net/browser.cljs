(ns maximoplus.net.browser
  (:require
   [goog.net.NetworkStatusMonitor :as nstat]
   [goog.net.ErrorCode :as ec]
   [goog.net.EventType :as gevt]
   [maximoplus.utils :as u]
   [goog.events]
   [goog.events.OnlineHandler]
   [maximoplus.arrays :as ar]
   [maximoplus.net.protocols :refer [INet]]
   [clojure.string :refer [blank?]]
   [maximoplus.promises :as p]
   )
  (:import [goog.net XhrIo]))

(def ^:dynamic *timeout* 10000)

(def tabsess (atom "123"))

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
     (.send this uri method content headers))))

(defn xhr-connection
  "Returns an XhrIo connection"
  []
  (XhrIo.))

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
        [(when-not (blank? resp-text)
           (try (u/transit-read resp-text)
                (catch js/Error e
                  (u/debug e)
                  nil)))
         (translate-goog-xhr-code (.getLastErrorCode xhr1))
         status]
        ))))

(defn callback-from-xhr
  [xhr1 callback error-callback]
;;  (u/debug "zovem callback from xhr " (. xhr1 (getLastErrorCode)))
  (if (= ec/NO_ERROR (. xhr1 (getLastErrorCode)))
    (callback (get-response-vector xhr1  ))
    (error-callback (get-response-vector xhr1))))
  
(defn send-meth [cf url callback error-callback & progress-callback]
  (let [xhr1 (xhr-connection)
        _ap  (if (= -1 (.indexOf url "?")) "?" "&")
        _url (str url _ap "t=" @tabsess)
        pc (if progress-callback (first progress-callback))]
  ;;  (u/debug "Zovem send-meth " url)
    (.setWithCredentials xhr1 true)
    (goog.events/listenOnce xhr1
                       "complete"
                       (fn[e]
                        ; (u/debug "xhr call for  " url " finished at " (.getTime (js/Date.)))
                         (callback-from-xhr xhr1 callback error-callback)
                         (.dispose xhr1)))
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

(defn send-get-with-timeout
  [url callback error-callback timeout]
  (try 
    (send-meth (fn [xhr1 url] (transmit xhr1 url "GET" nil nil timeout)) url callback error-callback)
    (catch js/Error e (u/debug "Error in AJAX call:" e))))

(def event-source (atom nil))

(defn sse-start
  [sse-path callback error-callback]
  (let [ev-s (js/EventSource. sse-path #js {:withCredentials true})]
    (reset! event-source ev-s)
    (.addEventListener ev-s "message"
                       (fn [message]
                         (let [_data (aget message "data")
                               data (if (= "" _data) "" (u/transit-read _data))]
               ;;            (u/debug "SSE" data)
                           (callback data))))
    (.addEventListener ev-s "error"
                       (fn [error]
                         ;;SSE doesn't give any specifics of the error, so error handler is not appropriate. Only time I saw it happning is on logout
         ;;                (u/debug "SSE error, probably session end")
           ;;              (.log js/console error)
                         (error-callback error)
                         ))))

(defn sse-stop
  []
  (when @event-source
    (.close @event-source)
    (reset! event-source nil)))

(defn ^:export listenOffline [cb]
  (let [oh (goog.events.OnlineHandler.)]
    (when-not (.isOnline oh)
      (cb true));ensure that the offline flag will be set to 1 immediately, so the controls don't depend on the event
    (goog.events/listen oh (js/Array. (.-ONLINE nstat/EventType) (.-OFFLINE nstat/EventType))
                        (fn []
                          (cb (not (.isOnline oh)))))))



(defn get-offline-db-from-server
  [dburl postqry]
  (p/get-promise
   (fn [resolve reject]
     (let [xhr1 (xhr-connection)]
       (.setWithCredentials xhr1 true)
       (goog.events/listenOnce xhr1
                               "complete"
                               (fn[e]
                                 (if (= ec/NO_ERROR (. xhr1 (getLastErrorCode)))
                                   (resolve (.getResponseText xhr1))
                                   (reject [ (translate-goog-xhr-code (.getLastErrorCode xhr1)) (.getResponseText xhr1)]))
                                 (.dispose xhr1)))
       (transmit xhr1 dburl "POST" postqry nil 0)))))


(deftype Browser []
  INet
  (-send-get
    [this url callback error-callback]
    (send-get url callback error-callback))
  (-send-get
    [this url callback error-callback data]
    (send-get-with-data url data callback error-callback))
  (-send-post
    [this url data callback error-callback]
    (send-post url data callback error-callback))
  (-send-post
    [this url data callback error-callback progress-callback]
    (send-post url data callback error-callback progress-callback))
  (-start-server-push-receiving
    [this sse-path callback error-callback]
    (sse-start sse-path callback error-callback))
  (-stop-server-push-receiving
    [this]
    (sse-stop))
  (-get-tabsess;;tabsess handling will be done by the implemntation (browser or node)
    [this]
    @tabsess)
  (-set-tabsess!
    [this _tabsess]
    (reset! tabsess _tabsess))
  (-send-get-with-timeout
    [this url callback error-callback timeout]
    (send-get-with-timeout url callback error-callback timeout))
  (-get-offline-db-from-server
    [this dburl postqry]
    (get-offline-db-from-server dburl postqry)))
