(ns maximoplus.net.rn
  (:require
   [maximoplus.net.protocols :refer [INet]]
   [maximoplus.utils :as u]
   [maximoplus.net.browser :as b]
   [rn-eventsource])
  )

;;(set! *warn-on-infer* true)


(def event-source (atom nil))

;;react native implements everything except SSE. Once they implement it, this is not necessary anymore

(deftype ReactNative []
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
    [this  sse-path callback error-callback]
    (let [^js/RNEventSource _event-source (js/RNEventSource. sse-path #js {:withCredentials true})]
      (reset! event-source _event-source)
      (.addEventListener _event-source "message"
                         (fn [message]
                           (let [_data (aget message "data")
                                 data (if (= "" _data) "" (u/transit-read _data))]
;;                             (u/debug "SSE" data)
                             (callback data))))
      (.addEventListener _event-source "error"
                         (fn [error]
                           (u/debug "SSE error, probably session end")
                           (error-callback error)
                           ))))
  (-stop-server-push-receiving
    [this]
    (when-let [^js/RNEventSource es @event-source]
      (.close es)
      (reset! event-source nil)))
  (-get-tabsess;;tabsess handling will be done by the implemntation (browser or node)
    [this]
    @b/tabsess)
  (-set-tabsess!
    [this tabsess]
    (reset! b/tabsess tabsess))
  )
