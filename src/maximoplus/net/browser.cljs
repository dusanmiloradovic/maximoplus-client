(ns maximoplus.net.browser
  (:require
   [goog.net.NetworkStatusMonitor :as nstat]
   [goog.net.ErrorCode :as ec]
   [goog.net.EventType :as gevt]
   [maximoplus.utils :as u]
   [goog.events]
   [goog.events.OnlineHandler]
   [maximoplus.arrays :as ar]
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
        [(try (u/transit-read resp-text)
              (catch js/Error e
                (u/debug e)
                nil))
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


(def ^:dynamic *run-the-longpoll* (atom false))
;this is set to false for easier start-up control from maximoplus.core package
;if it was true that would indicate the longpoll is already running, so it couldn't be started

(def ^:dynamic *longpoll-min* 5000)

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
                         (u/debug "SSE error, probably session end")
                        ; (error-callback error)
                         ))))

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


(defn stop-server-push-receiving []
  (if (exists? js/EventSource)
    (sse-stop)
    (reset! *run-the-longpoll* false)))

(defn start-server-push-receiving [sse-path longpoll-path force-long-poll? cb errb]
  (if (and (exists? js/EventSource) (not force-long-poll?))
    (sse-start sse-path cb errb)
    (when-not @*run-the-longpoll*
      (reset! *run-the-longpoll* true)
      (long-poll longpoll-path cb errb))))
