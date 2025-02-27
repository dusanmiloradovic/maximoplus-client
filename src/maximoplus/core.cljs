(ns maximoplus.core
  (:require     
   [maximoplus.net :as net]
   [maximoplus.offline :as offline]
   [cljs.reader :as reader]
   [clojure.set]
   [maximoplus.utils :as u]
   [maximoplus.db :as db]
   [maximoplus.arrays :as ar]
   [maximoplus.promises :as p]
   [cljs.core.async :as a :refer [put! chan promise-chan <! timeout close!]]
   )
  (:require-macros 
   [maximoplus.macros :as mm :refer [p-deferred-on p-deferred defcmd defcmd-with-prepare defcmd-post offline-alt-noobj offline-alt kk! kk-nocb! p!]]
   [cljs.core.async.macros :refer [go go-loop]]))

(declare yesnocancelproxy)
(declare page-init)
(declare get-column-metadata)
(declare update-mbovalue)
(declare update-mbovalue-row)
(declare get-id-from-row)
(declare put-pending-data)
(declare dispatch-upd)
(declare get-local-data)
(declare put-coll-data-attrval!)
(declare put-object-data-attrval!)
(declare get-row-from-id)
(declare start-receiving-events)
(declare get-local-data-all-attrs)
(declare get-parent-uniqueid)
(declare globalFunctions)


(declare offlinePrepareOne)
(declare offlineMetaMove)
(declare late-register)
(declare get-app-containers)

(enable-console-print!)

;;it is not always possible to know syncrhonously if the app is online or offline (important when starting app in offline mode(
(def offline-app-status (atom (p/get-deferred)))
(def is-offline (atom false));;few cases still required

(def PAGE-INIT-TIMEOUT 2000)

(defn get-server-channel
  []
  (let [ch (chan 1)]
    (net/send-get-with-timeout
     (net/init)
     (fn [ok]
       (go
         (>! ch true)
         (close! ch)))
     (fn [[_ err-type err-code]]
;;       (println "callback in go loop " err-type)
       (go
         (>! ch
             (if  (or
                   (= err-type "TIMEOUT")
                   (= err-type "OFFLINE")
                   (and
                    (= err-type "HTTP_ERROR")
                    (not= err-code 401)))
               false
               true))
         (close! ch)))
     PAGE-INIT-TIMEOUT)
    ch))

(declare setOffline)

(defn check-server-back-online
  []
;;  (println "enter check-server-back-onlne")
  (go-loop
      []
    (<! (timeout 10000))
    (let [is-online? (<! (get-server-channel))]
      (if is-online?
        (do
          (setOffline false)
          (.call (aget globalFunctions "notifyOffline") nil false))
        (recur)))))


(defn is-server-offline?
  []
  (p/get-promise
   (fn [resolve reject]
     (go
       (let [online (<! (get-server-channel))]
         (if-not online
           (when-not @is-offline
             (setOffline true)
             (.call (aget globalFunctions "notifyOffline") nil true)
             (check-server-back-online)
             )
           (when @is-offline
             (setOffline false)
             (.call (aget globalFunctions "notifyOffline") nil false)
             ))
         (resolve (not online)))))))


;;setting the offline status will be done from outside the core library

(declare page-init-called)
(declare page-init-channel)





(defn process-push-receiving-error
  [err]
  (is-server-offline?)
  )


(declare any-offline-enabled?)

(declare bulk-ev-dispf)
(declare error-dispf)

(defprotocol Component ;base protocol for the components
  (get-id [component])
  (debug-state [component])
  (get-state [component key])
  (toggle-state [component key value])
  (set-states [component kv])
  (get-container [component]);virtually all will have this
  (get-col-attrs [component])
  (remove-state [component key])
  (get-currow [component]) ;;temporarily move to this protocol, the whole container protocol should be moved to core
  (set-deferred [component])
  (remove-deferred [component])
  )

(defprotocol Offline
  (is-offline-enabled [container])
  (set-offline-enabled [container flag])
  (set-offline-enabled-nodel [container flag])
  (cont-late-register [control]);if the container was created offline and it goes online it needs to register on the server. Same goes if the session is expired during offline, and our login method keeps us on page without re-registering everything again
  (cont-late-register-init [control]);;after it was registered, init the data
  (get-offline-objects-tree [control])
  (get-offline-changes [control])
  (post-offl-changes [control cb errb])
  (save-offl-changes [control cb errb])
  (rollback-offl-changes [control cb errb])
  (offline-post-finished [control res])
  (display-offline-post-errors [control])
  )

(defn simple-receive [_channel f]
  (go
    (<! _channel);;ignore what is put, just delay until the completion of init
    (f)))

(def offline-posted (atom {}))
(def was-offline (atom false))
(def logged-in-chan (atom (promise-chan)))

;;this should be cleared when the device go offline. Once it is online, it should post the changes maximum once per table if it was not already posted.
;;MAYBE I need this just for the application containers (because the change is calculated recursively)

(defn ^:export setOffline 
  "This should be done automatically when the real physical offline happens."
  [offline]
  ;; (.call (aget globalFunctions "notifyOffline") nil offline)
  (if  offline
    (do
      (u/debug "Going offline")
      (net/stop-server-push-receiving)
      (swap! is-offline (fn [_] true))
      (reset! was-offline true)
      (reset! offline-posted {}))
    (if (= "123" (net/get-tabsess))
      (do;;if it was started when offine
        (u/debug "Going online")
        (net/stop-server-push-receiving)
        (swap! is-offline (fn [_] false))
        (.call (aget globalFunctions "global_login_function") nil [6 "Not Logged In" 401]))
      (do
        (u/debug "Going online")
        (net/start-server-push-receiving bulk-ev-dispf process-push-receiving-error)
        (swap! is-offline (fn [_] false))
        ;;if the offline period was brief, this will post the changes, otherwise if the session had
        ;;expired, it will go to the login function, and page-init will post the changes
        ;;to be sure, we have to check is the user logged in after he goes online
        (->
         (net/is-logged-in?)
         (p/then (fn [logged-in?]
                   (when logged-in?
                     (u/debug "Already logged in, posting offline changes")
                     (doseq [c (get-app-containers)]
                       (when (is-offline-enabled c)
                         (post-offl-changes c
                                            (fn [ok] (println "offline posting finished"))
                                            (fn [err]
                                              (u/debug "offline posting returned an error")
                                              ;;(println err)
                                              ))))))))))))

;;(defn listen-offline
;;  []
;;  (net/listen-offline (fn [offline]
;;                        (setOffline offline))))
                                        ;detects the offline and changes the status. TODO change the defcmd macro to report the error in case is-offline is set to true

(defn str->bool [x]
  (if (or (= true x) (= "true" x)) true false))

(defn remove-incl [a b]
  (loop [a a b b]
    (let [rf (first b)
          rs (rest b)]
      (if-not rf
        a
        (recur (remove #(= rf %) a) rs)))))

(defn remove-incl-arr [a b]
  (let [ret (ar/clone a)]
    (when-not (ar/empty? b)
      (dotimes [i (.-length b)]
        (let [to-remove (aget b i)
              ind (ar/index-of ret to-remove)
              ]
          
          (when-not (= -1 ind)
            (ar/remove-at! ret ind)
            ))))
    ret
    ));isto kao gore samo  za array

                                        ;(def object-metadata (atom {}))
(defn arr-obj-ind [arr o]
                                        ;za sada cu ovde da hardkodujem imena atributa, ako mi zatreba za nesto drugo, menjam fju
  (loop [cnt 0]
    (let [arro (aget arr cnt)]
      (if-not arro
        -1
        (if  (and (= (aget arro "control") (aget o "control")) (= (aget arro "action") (aget o "action"))) 
          cnt
          (if (> cnt (count arr))
            -1
            (recur (inc cnt))))))))

(def wait-actions (ar/empty))
(def prepared-actions (ar/empty))

(def container-promises (js-obj))
(def container-deferreds (js-obj))
                                        ;if the preparecall has been found it will not add the wait action, it is the job of the preparecall function. In this way we can have local wait actions(for example display the spinner in the place of the control)

(declare globalDisplayWaitCursor)
(declare globalRemoveWaitCursor)

(def page-init-called (atom false));;this will be set to true only once on calling the page init

(def page-init-channel (atom (promise-chan)));; this is the atom to the channel, and not just the channel, because on logout, we need to again re-initiate the page by calling the server, and getting the new tab session. When the session ends, and the login function is called the atom should again be reset to the new promise channel

(def page-opened (atom false));;page-init will be reset each time the logoff is called. THe new login procedure keeps the components on the client side, and do the late-register. The late register will be called only if page-opened is set to true


(defprotocol Receivable ; getting rid of events for propagate the server message, will use the core-async
  (process-received [component message])
  (send-message [component message components]);i can't think of anything else here but the direct children, but in any case might be useful
  (send-message-sync [component message components]);;core.async has a cost, stack size is growing rapidly. If there is no need, I will send message directly(synchronously to the chidlren
  (receive-message-sync [components message]);;this will be called on the child receiving the mesesage
  (receive-all-pending-sync [component]);;performance optimization, it will read all the messages once it receives the command
  (start-receiving [component])
  (stop-receiving [component])
  (get-receive-functions [component]);for each type of data received there may be a function to process it. If there is no function for the particular data type, the data is passed down to the child components, for its handlers to process
  (send-command [component command command-f command-cb command-errb]);;this shouuld replace the container command promises, all the callbacks will be processed via command channel
  )



(declare container-registry)

(extend-protocol Component
  string
  (toggle-state
    [component key value]
    (toggle-state (@container-registry component) key value)
    )
  (get-state
    [component key]
    (get-state (@container-registry component) key))
  (set-states
    [component kv]
    (set-states (@container-registry component) kv)
    )
  (get-container
    [component]
    (get-container (@container-registry component))
    )
  (debug-state
    [component]
    (debug-state (@container-registry component)))
  (get-currow
    [component]
    (get-currow (@container-registry component)))
  nil
  (toggle-state
    [component key value])
  (get-state
    [component key]
    nil)
  (set-states
    [component kv])
  (get-container
    [component]
    nil
    )
  (debug-state
    [component]
    nil)
  (get-currow
    [component]
    nil
    )
  )

(extend-protocol Offline
  string
  (set-offline-enabled
    [container flag]
    (when-let [cont (@container-registry container)]
      (set-offline-enabled cont  flag)))
  (set-offline-enabled-nodel
    [container flag]
    (when-let [cont (@container-registry container)]
      (set-offline-enabled-nodel cont flag))
    )
  (is-offline-enabled
    [container]
    (when-let [cont (@container-registry container)]
      (is-offline-enabled cont)
      )))

(defn any-offline-enabled?
  ;;if the offline is enabled for any container, then we will switch to offline mode when there is no network
  []
  (some
   is-offline-enabled
   (vals @container-registry)))

(defn add-prepare-action
  [control action]
  (when-let [container (get-container control)]
    (ar/conj! prepared-actions (js-obj "control" (get-id container) "action" action))))

(defn remove-prepare-action
  [control action]
  (when-let [container (get-container control)]
    (let [obj (js-obj "control" (get-id container) "action" action)]
      (when-let [ind (arr-obj-ind prepared-actions obj)]
        (ar/remove-at! prepared-actions ind)))))

(defn addWaitAction
  [control action]
  (let [obj (js-obj "control" (get-id control) "action" action)] 
    (when (= -1 (arr-obj-ind prepared-actions obj))
      (when (ar/empty? wait-actions)
        (.call (aget globalFunctions "globalDisplayWaitCursor"))
        )
      (if (= -1 (arr-obj-ind wait-actions obj))
        (ar/conj! wait-actions obj)))))

(defn removeWaitAction
  [control action]
  (let [obj (js-obj "control" (get-id control) "action" action)]
    (let [ind (arr-obj-ind wait-actions obj)]
      (when (not= -1 ind)
        (ar/remove-at! wait-actions ind)
        (when (ar/empty? wait-actions)
          (.call (aget globalFunctions "globalRemoveWaitCursor"))
          )))))


(def registered-columns (atom {}))

(def control-columns (atom {}))

(def container-registry (atom {}))

(def pending-messages
  (atom {}))

(def registered-components (atom {}));;in nodejs passing messages directly to visual components causes the stack overflow error. I will register the component here, and pass the messages in the background

(defn receive-pending-messages
  []
  (doseq [k (keys @pending-messages)]
    (let [comp (@registered-components k)]
      (receive-all-pending-sync comp)
))
  (reset! pending-messages {});;no race condition, this is happening on event loop
  )

(go-loop
    []
  (<! (timeout 20));;
  (receive-pending-messages);;put try/catch here when it becomes stable
  (recur)
  )




(defn add-container-to-registry [container]
  (swap! container-registry assoc (get-id container) container)
  )

(defn remove-container-from-registry [container]
  (swap! container-registry dissoc (get-id container))
  )

(def app-container-registry (atom {}));;for the offline reload, the starting point is the root, i.e app container

(defn add-app-container-to-registry [container]
  (swap! app-container-registry assoc (get-id container) container)
  )

(defn remove-app-container-from-registry [container]
  (swap! app-container-registry dissoc (get-id container))
  )

(defn get-registered-columns [container-name]
  (set
   (map #(.toUpperCase %) (@registered-columns container-name))))


(defn ^:export handleErrorMessage
  [text]
  (js/alert text)
  )

(defn ^:export globalErrorHandler
  [error-message error-code error-group error-key]
  (.call (aget globalFunctions "handleErrorMessage") nil error-message))



(defn ^:export globalCallbackHandler
  [val])

(defn ^:export globalDisplayWaitCursor
  []
;;  (println "Start Waiting");;wevery single implementation will have to do it, remove this ancient code below
;;  (.add (.. js/document -documentElement -classList) "waitcursor")
  )

(defn ^:export globalRemoveWaitCursor
  []
  ;;(println "Stop Waiting")
  ;;(.remove (.. js/document -documentElement -classList) "waitcursor")
  )

(defn ^:export globalPrepareCallHandler
  [action control]
  (addWaitAction control action)
  )

(defn ^:export globalFinishCallHandler
  [action control]
  (removeWaitAction control action))


;;TODO exc-handler se poziva iz macroa kada nisam prosledio errback handler. Treba to da zavrsim, da uvek uzima prvo u obzir lokalni hendler(niski prioriet)
(defn exc-handler [action-name [max-err-vec goog-xhr-code http-code]]
                                        ;  (u/debug exc)
  (if-not max-err-vec
    (.call (aget globalFunctions "globalErrorHandler") nil "Network Error" goog-xhr-code http-code)
    (if (= (nth max-err-vec 1) "Not logged in")
      (page-init)
      (let [[mx-error err err-group err-code] max-err-vec]
        (.call (aget globalFunctions "globalErrorHandler") nil err (name mx-error) err-group err-code)))))

(defn ^:export global-login-function
  [err]
  (set! (.-location js/window) "/login.html")) 

(defn ^:export logout []
  (net/send-get (net/logout)  page-init page-init)
  )

(def months
  ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])

(defn ^:export formatToMaximoDate [js-date]
  (let [day (.getDate js-date)
        month (.getMonth js-date)
        year (.getFullYear js-date)
        ]
    (str (goog.string/padNumber day 2) "-" (months month) "-" year)))

(def  logging-in (atom false))

(defn ^:export max-login[username password okf errf]
  (.call (aget globalFunctions "globalDisplayWaitCursor"))
  (let [post-data (str "username=" username "&password=" password)]
    (net/send-get (str (net/login) "?" post-data)
                  (fn [ok]
                    (.call (aget globalFunctions "globalRemoveWaitCursor"))
                    (swap! logging-in (fn [_] false))
                    (page-init)
                    ( okf ok))
                  (fn [err]
                    (.call (aget globalFunctions "globalRemoveWaitCursor"))
                    (swap! logging-in (fn [_] false))
                    ( errf err)))))

(defn ^:export redirect-after-login
  []
  "index.html"
  )

(defn ^:export max-login-with-redirect [username password]
  (max-login username password
             #(let [red-uri (first %)
                    refer (.-referrer js/document)]
                (swap! logging-in (fn [_] false))
                (set! (.-location js/window) (if (and refer (-> refer empty? not)) refer (redirect-after-login))))
             (fn[e] (swap! logging-in (fn [_] false))(u/debug e) (js/alert "Invalid username or password"))))



(defn ^:export general-max-login [credentialsObject okf errf]
  (.call (aget globalFunctions "globalDisplayWaitCursor"))
  (net/send-get  (str (net/general-login) "?"
                      (str "credentials=" (u/create-json credentialsObject)))
                 (fn [ok]
                   (.call (aget globalFunctions "globalRemoveWaitCursor"))
                   (swap! logging-in (fn [_] false))
                   (page-init)
                   ( okf ok))
                 (fn [err]
                   (.call (aget globalFunctions "globalRemoveWaitCursor"))
                   (swap! logging-in (fn [_] false))
                   ( errf err))))

(defn ^:export general-max-login-with-redirect [credentialsObject]
  (general-max-login credentialsObject
                     #(let [red-uri (first %)
                            refer (.-referrer js/document)]
                        (swap! logging-in (fn [_] false))
                        (set! (.-location js/window) (if refer refer "/")))
                     (fn[e] (swap! logging-in (fn [_] false))(u/debug e) (js/alert "Invalid username or password"))))

(def peer-controls-mutable (ar/empty))

(defn add-peer-control [registered new]
  (if registered
    (dotimes [i (.-length peer-controls-mutable)]
      (let [cpeers (aget peer-controls-mutable i)]
        (dotimes [j (.-length cpeers)]
          (let [cp (aget cpeers j)]
            (when (= cp registered)
              (ar/conj! cpeers new))))))
    (let [newar (ar/empty)]
      (ar/conj! newar new)
      (ar/conj! peer-controls-mutable newar))))

(defn get-peer-controls [cid]
  (loop [i 0]
    (when-not  (= i (.-length peer-controls-mutable))
      (let [curar (aget peer-controls-mutable i)]
        (if-not (= -1 (ar/index-of curar cid))
          curar
          (recur (inc i)))))))

(defn is-registered? [control-name]
  (get-peer-controls control-name))


(defn dispatch-data! [component data-type data];dispatch synchronously!
  (receive-message-sync component {:type data-type :data data}))

(defn dispatch-peers! [control event data & exclude-source?]
  (let [peers (get-peer-controls control)
        pc (if exclude-source? (ar/triml peers 1) peers);don't send the event to the original
        ]
    (when pc
      (dotimes [i (.-length pc)]
        (let [p (aget pc i)]
          (when (is-registered? p)
            (when-let [crp (@container-registry p)]
              (dispatch-data!  crp event data))))))))

(declare get-control-metadata)
                                        ;(declare add-pending-message)

(def rel-map (js-obj))
(def rel-map-reverse (js-obj))

(defn ^:export moveToOffline
  [rel-name uniqueId parentId dta]
  (offline/moveToOffline rel-name (assoc dta "uniqueid" uniqueId "parentid"
                                         (if (.startsWith rel-name "list_") -1  parentId)) ))

;;(defn prepare-flags-for-offline
;;  [flags]
;;  (into {}
;;        (for [[k v] flags]
;;          [k (u/transit-json v)])))

(defn ^:export moveFlagsToOffline
  [rel-name uniqueId parentId flgs]
  (offline/moveFlagsToOffline rel-name (assoc
                                        flgs
;;                                        (prepare-flags-for-offline flgs)
                                              "uniqueid" uniqueId "parentid" parentId) ))

(defn add-relationship [name _object parent-name]
  (let [object (.toLowerCase _object)]
    (if-not parent-name
      (do
        (aset rel-map name object)
        (aset rel-map-reverse object name))
      (let [parent-obj (aget rel-map parent-name)
            r-obj-name (str parent-obj "_" object)
            rls (get-state name :rel-containers)]
        (aset rel-map name r-obj-name)
        (aset rel-map-reverse r-obj-name name)))))

(defcmd-with-prepare register-main-mboset
  [control-name main-object]
  (do
    (add-relationship control-name main-object nil))
  (fn [evt]
    (let [resp (first evt)
          already-reg (first resp)
          ]
      (add-peer-control nil control-name)
      (toggle-state control-name :mainmboset true)
                                        ;the purpose of this flag is to register the controls when going from offline to online, if the controls were not really registered before, just offline , or if it was registered online, and then session expired when offline. Library will send the register commands automatically starting with the main set and then going recursivelly for all the rel containers
      )))

(defn offline-register-main-mboset
  "there is no need to check for existence of tables, it will be fail on add-control-columns if the table is not available offline"
  [control-name main-object cb errb]
  (add-relationship control-name main-object nil)
  (add-peer-control nil control-name)
  (toggle-state control-name :mainmboset true)
  (toggle-state control-name :registered-from-offline true)
  (when cb (cb "done")))

(offline-alt-noobj register-main-mboset-with-offline register-main-mboset offline-register-main-mboset [control-name main-object])

(defn ^:export register-mainset
  [control-name main-object cb errb]
  (register-main-mboset-with-offline control-name main-object cb errb))

(defcmd register-maximo-menu;;not realy used anymore
  [control-name]
  (fn [evt]
    (let [resp (first evt)
          already-reg (first resp)
          ]
         (add-peer-control nil control-name))))

(declare offline-insert-qbe)

(defcmd set-qbe [control-name qbe-attr qbe-expr]
  (fn [evt]
    (let [qbe (-> (get evt 0)  u/vec-to-map)
          qbe-existing (-> (get-state control-name :qbe) u/vec-to-map)
          qbe-res (merge qbe-existing qbe)
          qbe-res-vec (reduce-kv (fn [m k v] (merge m k v)) [] qbe-res)]
      (toggle-state control-name :qbe qbe-res-vec)
      (when (is-offline-enabled control-name)
        (offline-insert-qbe control-name (first evt))))))

(defn offline-set-qbe [control-name qbe-attr qbe-expr cb errb]
  (->
   (offline/insert-offline-qbe (aget rel-map control-name) qbe-attr qbe-expr)
   (p/then (fn [ok] (when cb (cb ok))))))

(offline-alt-noobj set-qbe-with-offline set-qbe offline-set-qbe [control-name qbe-attr qbe-expr])

(declare overwrite-metadata)

(defcmd add-control-columns [control-name columns]
  (fn[evt]
    (let [metadata  (first evt)]
      (overwrite-metadata control-name metadata))))

(defn offline-add-control-columns [control-name object-name columns cb errb]
  (->
   (offline/get-column-names object-name)
   (p/then
    (fn [existing-columns]
      (if-not 
          (empty?
           (clojure.set/difference (set (map #(.toUpperCase %) columns))
                                   (set existing-columns)))
        (errb "Columns don't exist offline, not able to register columns ")
        (->
         (offline/get-object-meta object-name)
         (p/then
          (fn [metadata]
            (let [cmeta (js->clj (aget metadata "columnsMeta") :keywordize-keys true)]
              (overwrite-metadata control-name cmeta)
              (cb cmeta))))))))
   (p/then-catch
    (fn [err]
      (let [formatted-err [[:js (aget err "message") nil nil] 0 0]]
        (if errb
          (errb formatted-err)
          (exc-handler nil formatted-err)))
      err))))

(offline-alt add-control-columns-with-offline add-control-columns offline-add-control-columns [control-name columns])

(defcmd delete [control-name])

(defcmd undelete [control-name])

(defn offline-delete [control-name cb errb]
  (->
   (get-parent-uniqueid control-name)
   (p/then (fn [puid] (offline/row-delete (aget rel-map control-name) puid (get-currow control-name))))
   (p/then (fn [ok] (when cb (cb ok))))))

(defn offline-undelete [control-name cb errb]
  (->
   (get-parent-uniqueid control-name)
   (p/then (fn [puid] (offline/row-undelete (aget rel-map control-name)  puid (get-currow control-name))))
   (p/then (fn [ok] (when cb (cb ok))))))

(offline-alt-noobj delete-with-offline delete offline-delete [control-name])

(offline-alt-noobj undelete-with-offline undelete offline-undelete [control-name])

(defcmd remove-control-columns [control-name columns])

(defcmd get-qbe [control-name]
  (fn [evt]
    (let [qbe (-> (get evt 0)  u/vec-to-map)
          qbe-existing (-> (get-state control-name :qbe) u/vec-to-map)
          qbe-res (merge qbe-existing qbe)
          qbe-res-vec (reduce-kv (fn [m k v] (merge m k v)) [] qbe-res)]
      (toggle-state control-name :qbe qbe-res-vec)
      (when (is-offline-enabled control-name)
        (offline-insert-qbe control-name (first evt))))))

(defcmd get-columns-qbe [control-name columns]
  (fn [evt]
    (when (is-offline-enabled control-name)
      (offline-insert-qbe control-name (first evt)))))

(defcmd set-order-by [control-name column]
  (fn [evt]
    (toggle-state control-name :orderby column)))

(defcmd set-current-app [control-name app])

(defn offline-set-current-app [control-name app cb errb]
  (when cb (cb app)))

(offline-alt-noobj set-current-app-with-offline set-current-app offline-set-current-app [control-name app])

(defcmd set-unique-app [control-name app unique-id])

(defcmd set-unique-id [control-name unique-id])

(defcmd run-mbo-command [control-name command arg-control])

(defcmd access-to-option [control-name option])

(defcmd run-mboset-command [control-name command arg-control])

(defcmd command-on-selection [control-name command])

(defcmd mboset-count [control-name])

(def option-descriptions (atom {}))

(defcmd get-option-descriptions [control-name]
  (fn[evt]
    (swap! option-descriptions assoc control-name  (first evt))
    ))

(defn get-option-description
  [control option]
  (when-let [ocd (@option-descriptions control)]
    (aget ocd (.toUpperCase option))))


(defn process-register-list-callback-event [list-name evt]
  (let [resp  (first evt)
        already-reg (first resp)
        ]
    (if (= "none" already-reg)
      (add-peer-control nil list-name)
      (add-peer-control already-reg list-name))))

(defcmd-with-prepare register-list [list-name mbocontainer-name column-name force-qbe?]
  (when-let [cont-obj-name (aget rel-map mbocontainer-name) ]
    (add-relationship list-name (str "list_" (.toLowerCase cont-obj-name) "_" (.toLowerCase column-name)) nil))
  (fn[evt]
    (process-register-list-callback-event list-name evt)))

(defn exist-offline-list? [mbocontainer-name column-name]
  (offline/exist-table? (str "list_" (.toLowerCase (aget rel-map mbocontainer-name)) "_" (.toLowerCase column-name) )))

(defn exist-table? [mbocontainer-name & raw?]
  (offline/exist-table? (aget rel-map mbocontainer-name) (first raw?))
  )

(defn offline-register-list [list-name mbocontainer-name column-name force-qbe? cb errb]
  (->
   (offline/exist-table? (str "list_" (.toLowerCase (aget rel-map mbocontainer-name)) "_" (.toLowerCase column-name)))
   (p/then
    (fn [ex?]
      (if-not ex?
        (errb "No offline list exists for this column")
        (do
          (add-peer-control nil list-name)
          (add-relationship list-name (str "list_" (.toLowerCase (aget rel-map mbocontainer-name)) "_" (.toLowerCase column-name)) nil)
          (cb "done")))))))

(offline-alt-noobj register-list-with-offline register-list offline-register-list [list-name mbocontainer-name column-name force-qbe?])

(defcmd register-qbe-list [list-name mbocontainer-name  column-name]
  (fn[evt] (process-register-list-callback-event list-name evt))
  )

(defcmd get-key-attributes [control-name])

(defcmd smart-fill [fill-list-name mbocontainer-name column-name value]
  (fn[evt] (process-register-list-callback-event fill-list-name evt)))

(defcmd set-value-from-list [mbocontainer-name list-name column-name]
  (fn [evt]
    (let [currow (get-currow mbocontainer-name)
          val (first evt)]
      (dispatch-upd mbocontainer-name currow column-name val))))

(declare offline-set-value)

(defn offline-set-value-from-list [mbocontainer-name list-name column-name cb errb]
  (->
   (offline/getReturnListValue (aget rel-map list-name) (get-currow list-name)
                               (get (get-state mbocontainer-name :offlineReturnColumns) (.toUpperCase column-name)))
   (p/then
    (fn [val]
      (offline-set-value mbocontainer-name (aget rel-map mbocontainer-name) column-name val nil errb)))
   (p/then
    (fn [val]
      (when cb (cb val))
      val))))

(offline-alt-noobj set-value-from-list-with-offline set-value-from-list offline-set-value-from-list [mbocontainer-name list-name column-name])

(defcmd set-qbe-from-list [mbocontainer-name list-name column-name])

(defn offline-set-qbe-from-list [mbocontainer-name list-name column-name cb errb]
  (->
   (offline/getReturnListValue (aget rel-map list-name) (get-currow list-name)
                               (get (get-state mbocontainer-name :offlineReturnColumns) (.toUpperCase column-name)))
   (p/then
    (fn [val]
      ;;the difference here is that qbe may consist of multiple values, so when user picks two lines, qbe should read them both (_selected flag is Y)
      (offline-set-value mbocontainer-name (aget rel-map mbocontainer-name) column-name val nil errb)))
   (p/then
    (fn [val]
      (when cb (cb val))
      val))))

(declare get-connected-control)

(defn is-virtual? [column-name]
  (= (.toUpperCase column-name) "_SELECTED")
  )
;;BIG change. After the login, if the user was online, but logged out, the constructor of the
;;visual components wasn't called, so the message channel is never initialized
;;we will include promise channel that will be called when the user is logged in or is offline.
;;when the login dialog comes, the reset of the promise chan



(defn register-columns [container columns-all cb-handler errback-handler]
  (let [columns (filter (comp not is-virtual?) columns-all)
        container-name (get-id container)]
    (let [cbh (fn [ok]
                (let [old-cols (set (@registered-columns container-name))
                      new-cols (set (map #(.toUpperCase %) columns))
                      tc (vec (clojure.set/union old-cols new-cols))]
                  (swap! registered-columns assoc container-name tc))
                (when cb-handler (cb-handler ok))
                (when (and
                       (not (get-state container :dettached))
                       (is-offline-enabled container) 
                       (not @is-offline))
                  (offlinePrepareOne (get-id container))))
          errbh (fn [err]
                  (when errback-handler (errback-handler err)))]
      (kk! container "registercol" add-control-columns-with-offline columns-all cbh errbh))))
      


(defn deregister-columns [container-name columns]
  (let [newv (remove-incl (@registered-columns container-name) columns)
        diff (clojure.set/difference (set (@registered-columns container-name)) (set newv))
        ]
    (swap! registered-columns assoc container-name newv)
    (when-not (empty? diff)
      (remove-control-columns container-name (vec diff)))))

(defn is-registered-column? [container-name column]
  (if (= column "_SELECTED")
    true
    (get-column-metadata container-name column)))

(defcmd move-to [control-name row])

                                        ;(defcmd set-value [control-name attribute value])

                                        ;ovo radim zato sto setovanje long description-a ne okida set-value za isti mbo, a ne zelim da radim ovo samo za long description
(defcmd-with-prepare set-value [control-name attribute value]
  (let [currow (get-currow control-name)]
    (dispatch-upd control-name currow attribute value))
  (fn [evt]
    (let [val (first evt)
          currow (get-currow control-name)]
      (put-object-data-attrval! control-name currow attribute value)
      (dispatch-upd control-name currow attribute val)));kada je sve ok treba da apdjetujem local storage, jer se za slucaj ld to ne desava
  (fn [err]
    (let [currow (get-currow control-name)]
      (dispatch-upd control-name  currow attribute (get-local-data control-name  currow attribute)))))

(defn offline-set-value [control-name rel-name attribute value cb errb]
  (let [currow (get-currow control-name)]
    (dispatch-upd control-name currow attribute value)
    (->
     (get-parent-uniqueid control-name)
     (p/then (fn [puid] (offline/set-value rel-name puid currow attribute value)))
     (p/then
      (fn [_]
        (put-object-data-attrval! control-name currow attribute value)
        (when cb (cb value))
        value))
     (p/then-catch
      (fn [err] 
                                        ;TODO setting the offline validators that reject the promise
        (dispatch-upd control-name  currow attribute (get-local-data control-name  currow attribute))
        (when errb (u/debug err) (errb err)))))))

(offline-alt set-value-with-offline set-value offline-set-value [control-name attribute value])


(defcmd set-zombie [control-name attribute value])



(defcmd save [control-name])

(defcmd forward [control-name] (fn [evt]
                                 (toggle-state control-name :firstrow false)
                                 (toggle-state control-name :lastrow (not= "ok" (first evt)))))

(defcmd backward [control-name] (fn [evt]
                                     (do
                                       (toggle-state control-name :lastrow false)
                                       (toggle-state control-name :firstrow (not= "ok" (first evt))))))

(defcmd add-at-end [control-name]
  (fn [evt]
    (let [rd-evt  (first evt)]
      (dispatch-peers! control-name "add-at-end" {:row (first rd-evt) :data (second rd-evt)})
      rd-evt
      ))
  )


(defcmd add-at-index [control-name ind]
  (fn [evt]
    (let [rd-evt  (first evt)] 
      (dispatch-peers! control-name "add-at-index" {:row (first rd-evt) :data (second rd-evt)})
      )))

(defn offline-add-at-index [control-name ind cb errb]
  (->
   (get-parent-uniqueid control-name)        
   (p/then (fn [puid] (offline/add-at-index (aget rel-map control-name) puid  ind)))
   (p/then (fn [ok] (when cb (cb ok))))))

(offline-alt-noobj add-at-index-with-offline add-at-index offline-add-at-index [control-name ind])


(declare put-object-flags!)
(declare put-pending-flags!)

(defn update-flags-with-fetched [control-name [row flags]]
  (put-object-flags! control-name (js/parseInt row) flags))

(def object-data (atom {}))
(def pending-data (atom {}))
;;(def pending-messages (atom {}))
;;when fetch of data is done, still it misses the links between rownum and id. Until that is known it remains pending

(defn put-coll-data! [coll-name control-name cid data-or-flags ttype]
  ;;ttype may be :data or :flags
  ;;  {:pre (or (= :data ttype) (= :flags ttype))}
  (when data-or-flags
    (swap! coll-name 
           (fn [coll]
             (update-in coll [control-name  cid]
                        (fn [ex]
                          (let [_dta (ttype ex)]
                            (assoc ex ttype
                                   (loop [keys-data (keys data-or-flags) dta (transient (if _dta _dta {}))]
                                     (let [_k (first keys-data)]
                                       (if-not _k
                                         (persistent! dta)
                                         (recur (rest keys-data)
                                                (assoc! dta _k (data-or-flags _k))))))))))))))

(defn put-coll-data-attrval! [coll-name control-name cid attribute value ttype]
  (swap! coll-name 
         (fn [coll]
           (update-in coll [control-name cid]
                      (fn [ex]
                        (let [_dta (ttype ex)]
                          (assoc ex ttype (assoc _dta attribute value))))))))

(defn put-object-data! [control-name row data]
  (put-coll-data! object-data control-name row data :data))

(defn put-pending-data! [control-name cid data]
  (put-coll-data! pending-data control-name cid data :data))

(defn put-object-flags! [control-name row data]
  (put-coll-data! object-data control-name row data :flags))

(defn put-pending-flags! [control-name row data]
  (put-coll-data! pending-data control-name row data :flags))


(defn put-object-data-attrval! [control-name row attr val]
  (put-coll-data-attrval! object-data control-name row attr val :data))

(defn put-pending-data-attrval! [control-name cid attr val]
  (put-coll-data-attrval! pending-data control-name cid attr val :data))

(defn put-pending-flags-attrval! [control-name cid attr val]
  (put-coll-data-attrval! pending-data control-name cid attr val :flags))

(defn put-object-flags-attrval! [control-name row attr val]
  (put-coll-data-attrval! object-data control-name row attr val :flags))

                                        ;this is used just for the pending data. pending data should be updated just if no attribute data is found in object data (because it came before the data that was fetched, or updated with the rownum)


(defn move-data-and-flags-from-pending [control-name mbo-id]
  (when-let [row (get-row-from-id control-name  mbo-id)]
    (let [{p-data :data p-flags :flags} 
          (get (@pending-data control-name) mbo-id)]
      (put-object-data! control-name row p-data)
      (put-object-flags! control-name row p-flags)
      (swap! pending-data
             (fn [p]
               (update-in p [control-name] dissoc mbo-id))))))

(defn get-local-flags [control-name row]
  (-> (@object-data control-name) (get row) :flags))

(defn get-local-flag [control-name row column]
  (-> (@object-data control-name) (get row) :flags  (get column)))

(defn get-local-data [control-name row column]
  (let [od (@object-data control-name)]
    (->  od (get row) :data (get column))))

(defn get-local-data-fetch-size
  [control-name]
  (-> (@object-data control-name) count))

(defn get-local-data-all-attrs [control-name row]
  (-> (@object-data control-name) (get (js/parseInt row))))

(defn flag-read-only? [flag]
  (not= 0 (bit-and 7 (js/parseInt flag))))

(defn flag-required? [flag]
  (not= 0 (bit-and 8 (js/parseInt flag))))

(declare get-curr-uniqueid-promise)

(defn get-fetched-row-data
  [rd-evt]
  ;;this will be used by graphql implementations, where querying will immediately return the data. Subscriptions will still use the same mechanism
  (let [rownum (js/parseInt (nth rd-evt 0))
        df (nth rd-evt 1);data and flags
        columns (keys df)
        _dta (transient {})
        _flg (transient {})
        ]
    (when-not (js/isNaN rownum)
      (loop [ks (keys df) _dta _dta _flg _flg]
        (if (empty? ks)
          [rownum (persistent! _dta) (persistent! _flg)]
          (let [column (first ks)
                _val (df column)
                rd-data (when-not (keyword? column) (if (vector? _val) (nth _val 0) _val))
                rd-flags (when-not (keyword? column) (when (vector? _val) (nth  _val 1)))]
            (recur (rest ks)
                   (if-not rd-data _dta (assoc! _dta column  rd-data))
                   (if-not rd-flags _flg (assoc! _flg column [(flag-read-only? rd-flags) (flag-required? rd-flags) ])))))))))

(declare get-first-non-single)

(defn get-offline-insert-rownum
  [containerid]
  (get-state
   (get-first-non-single containerid)
   :currrow))

(defn fetched-row-callback [control-name rd-evt & offline?]
  (when (first rd-evt)
    (let [[rownum _dta flg] (get-fetched-row-data rd-evt)
          dta (if-not (get _dta "_uniqueid")
                (assoc _dta "_uniqueid" (get _dta "uniqueid"));;happens when fetching starting from offline
                _dta)
          _rownum (if-not
                     (get-state control-name :singlembo)
                    rownum
                   (get-offline-insert-rownum control-name))]
      (put-object-data! control-name rownum dta)
      (put-object-flags! control-name rownum flg)
      (dispatch-peers! control-name "fetched-row" {:row rownum :data dta :flags flg })
      (when-let [d (get-curr-uniqueid-promise control-name rownum)]
        (p/callback d (get dta "_uniqueid")))
      (when (and
             (not offline?)
             (is-offline-enabled control-name))
        "dont move to offline storage if already offline"
        (let [rel-name (aget rel-map control-name)
              o-dta (assoc dta "rownum" _rownum)
              o-flags (assoc flg "rownum" _rownum)]
          (p/then
           (get-parent-uniqueid control-name)
           (fn [parent-uniqueid]
             (moveToOffline rel-name (get dta "_uniqueid") parent-uniqueid o-dta)
             (moveFlagsToOffline rel-name (get dta "_uniqueid") parent-uniqueid o-flags))))))))

(defcmd fetch [control-name]
  (fn [evt]
    (let [rd-evt  (first evt)] 
      (fetched-row-callback control-name rd-evt)
      )))

(defcmd fetch-current [control-name]);;no need to update the local data here, this will be used just in GraphQL

(defcmd fetch-no-move [control-name]
  (fn [evt]
    (let [rd-evt  (first evt)] 
      (fetched-row-callback control-name rd-evt)
      )))

(defn fetch-multi-rows-offline [control-name rel-name start-row num-rows cb errb]
  (->
   (get-parent-uniqueid control-name)
   (p/then
    (fn [parent-id]
      ;;assume the list will be stored once, not multiple times, for each parent(complex list must be handled from the frontend
      (offline/fetch-multi-rows rel-name (if (.startsWith rel-name "list_") -1 parent-id) start-row num-rows)))
   (p/then
    (fn [rows]
      (doseq [row rows]
        (fetched-row-callback control-name row true))
      (dispatch-peers! control-name "fetch-finished" {})
      (when cb (cb rows))))
   (p/then
    (fn [_]
      (offline/get-lists false)
      ))
   (p/then (fn [lists]
             (offline/remove-selection (filter #(.startsWith % (str "list_" (.toUpperCase rel-name)))
                                           lists))))
   (p/then-catch
    (fn [err]
      (let [formatted-err [[:js (aget err "message") nil nil] 0 0]]
        (if errb
          (errb formatted-err)
          (exc-handler nil formatted-err)))))))


(defcmd fetch-multi-rows [control-name start-row num-rows]
  (fn [evt]
    (when-not (= "norow" (first evt))
      (doseq [rd-evt  (first evt)]
        (fetched-row-callback control-name rd-evt))
      (dispatch-peers! control-name "fetch-finished" {}))
    (fn [err])))

(defcmd fetch-multi-rows-no-reset
  [control-name start-row num-rows]
  (fn [evt]
    (when-not (= "norow" (first evt))
      (doseq [rd-evt  (first evt)]
        (fetched-row-callback control-name rd-evt))
      (dispatch-peers! control-name "fetch-finished" {}))
    (fn [err])))

(defcmd multi-select [control-name value start-row num-rows])

(defn loop-cmd
  "loops the commands with the callback. we need to put the commands as defined by macro as argument"
  [control-name command iterations]
  (loop [iterations (dec iterations)  acc (fn [_] (command control-name))]
    (if (= 0 iterations)
      acc
      (recur (dec iterations) (fn [_] (command control-name acc))))))

(defn get-connected-control [control-name]
  "One control is the master among the peers in a sense it is connected to the Maximo. Others are just using the data"
  (if-let [pc (get-peer-controls control-name)]
    (aget pc 0)
    control-name
    ))

(defn is-main-peer [control-name]
  (= control-name (get-connected-control control-name)))



(defn get-id-from-row [control-name row]
  (-> (@object-data control-name) :idsmap (get (js/parseInt row))))

(defn get-row-from-id [control-name id]
  (-> (@object-data control-name) :rowsmap (get id)))

(defn- remove-after-row!
  [control-name rownum]
  (swap! object-data 
         (fn [of]
           (assoc-in of [control-name]
                     (loop [dta (-> of control-name transient) r (js/parseInt rownum)]
                       (if-not (dta r) 
                         (persistent! dta)
                         (recur (dissoc! dta r) (inc r))))))))

(defn trim-object-data
  "when adding the new row we need to reorganize the data, easiest is to trim the existing one and re-fetch"
  [control-name row]
  (remove-after-row! control-name row)
  )


(defn rows-in-local?
  [control-name start-row no-rows]
  (when-let [locrows (@object-data control-name)]
    (let [reg-cols (set
                    (map #(.toUpperCase %)
                         (@registered-columns control-name)))
          cmp-cols? (fn [_cols]
                      (when reg-cols
                        (empty?
                         (clojure.set/difference reg-cols (set _cols)))))]
      (loop [_rw start-row]
        (if (= _rw (+ start-row no-rows))
          reg-cols
          (let [_ldr (:data (get locrows _rw)) ]
            (if-not
                (and _ldr (cmp-cols? (keys _ldr)))
              false
              (recur (inc _rw)))))))))

(declare fetch-multi-rows-with-offline)

(defn fetch-with-local [control-name row  no-rows & callbacks]
  (let [rows-to-fetch (if no-rows (js/parseInt  no-rows) 1)
        mctl (get-connected-control control-name)
        r (js/parseInt row)
        nrs (js/parseInt  no-rows)
        cb (first callbacks)
        errb (second callbacks)
        ]
;    (u/debug "fetch with local " control-name " from " row " no rows" no-rows)
    (assert (and row no-rows) "Fetching must have the starting row and the number of rows specified")
    (when-not (= -1 r)
      (if-let [reg-cols (rows-in-local? mctl r nrs)] ;;returns registered cols for performance optimization
        (do
          (doseq [x (range  r (+ r nrs))]
            (let [{data :data flags :flags} (get (@object-data control-name) x)
                  dfgs {:row x :data (select-keys data reg-cols) :flags (select-keys flags reg-cols)}]
              (dispatch-peers! mctl "fetched-row" dfgs)))
          (dispatch-peers! control-name "fetch-finished" {})
          (when cb (cb "ok")))
        (fetch-multi-rows-with-offline mctl r nrs cb errb)))))

(defcmd reset [control-name] (fn [x]
                                  ))

(declare reset-controls)

(defn offline-reset [control-name cb errb]
  (reset-controls [control-name])
  (when cb (cb "done"))
  )

(offline-alt-noobj reset-with-offline reset offline-reset [control-name])


(defn get-column-metadata
  [control-name column]
  (first (filter #(= (.toUpperCase column) (:attributeName  %))
                 (-> (@object-data control-name)  :metadata))))


(defn overwrite-metadata
  [control-name metadata]
  (swap! object-data (fn [o];assoc-in, because it overwrites
                       (assoc-in o [control-name :metadata]
                                 (conj metadata {:attributeName  "_SELECTED" :maxType "YORN" })))))

(defn update-metadata-attribute!
  [control-name attribute meta-name meta-value]
  (swap! object-data
         (fn [o]
           (assoc-in o [control-name :metadata]
                     (map
                      (fn [m] (if (= (:attributeName m) (.toUpperCase attribute))
                                (assoc m meta-name meta-value)
                                m
                                ))
                      (:metadata (@object-data control-name))))))) 

(declare add-col-attr)
(declare add-col-attrs)

(defn add-virtual-column-to-metadata
  [control column metadata]
  (swap! object-data
         (fn [o]
           (update-in o [(-> control get-container get-id) :metadata]
                      conj (assoc metadata :attributeName  column :maxType "VIRTUAL")))))


(defn add-col-attr
  ;;metadata is on the container level, we need to have it also on the level of the control
  [control column attribute-name attribute-value]
  ;;we can pass the name of the column or the column itself

  (let [_cup (if (goog/isString column)
               (.toUpperCase column)
               (let [_mda (aget column "metadata")]
                 (get _mda "attributeName")))]
    (when (= :offlineReturnColumn attribute-name)
      (toggle-state (get-container control)
                    :offlineReturnColumns (assoc
                                           (get-state (get-container control) :offlineReturnColumns)
                                           _cup attribute-value
                                           )))
    (swap! (aget control "state")
           (fn [s]
             (update-in s [:colAttrs _cup] assoc attribute-name attribute-value)))))
;; i will leave the metadata on the control level to be the javascript object, because the user may want to read it directly))


(defn add-col-attrs
  [control column kv]
  (doseq [[k v] (js->clj kv)]
    (add-col-attr control column k v)))


(defn remove-col-attr
  [control column attribute-name]
  (let [_cup (if (goog/isString column)
               (.toUpperCase column)
               (let [_mda (aget column "metadata")]
                 (get _mda "attributeName")
                 ))]
    (swap! (aget control "state")
           (fn [s]
             (update-in s [:colAttrs _cup] dissoc attribute-name )))))

(declare get-attribute-metadata)



(defn get-attribute-metadata-with-col-attrs
  [control _column]
  (let [column (.toUpperCase _column)
        cont (get-id (get-container control))
        _attr-metadata (get-column-metadata  cont column)
        col-attr-metadata (get (get-col-attrs control) column)
        attr-name-virt (get col-attr-metadata "attributeName")
        attr-metadata (if _attr-metadata _attr-metadata (when attr-name-virt (get-column-metadata cont attr-name-virt) ))]
    (merge attr-metadata col-attr-metadata)))

(defn get-column-attribute
  [control column attribute]
  (let [all-meta (get-attribute-metadata-with-col-attrs control column)]
    (get all-meta attribute)))

(defn dispatch-upd [control-name  rownum column value]
  (when (is-registered-column? control-name column)
    (dispatch-peers! control-name "update-control-data" { :control-name control-name :column column :value value :rownum rownum})))

(defn update-mbovalue-row 
  [control-name row k v]
  (put-object-data-attrval! control-name row k v)
  (dispatch-upd control-name  row k v)
  )

(defn update-mbovalue [ev]
  (let [control-names (nth  ev 0)
        cid (nth ev 1)
        v (nth ev 2)
        valid (nth v 0)
        valval (nth v 1)
        ]
    (doseq [control-name control-names]
      (if-let [_row (get-row-from-id control-name cid)]
        (do
          (put-object-data-attrval! control-name _row valid valval)
          (dispatch-upd control-name _row valid valval))
        (put-pending-data-attrval! control-name cid valid valval)
        ))))

(defn update-control-attribute-flags [x]
  (let [command (nth x 0)
        ctrls (nth x 1)
        mboid (nth x 2)
        flinf (nth x 3)]
    (doseq [c ctrls]
      (when-let [rownum (get-row-from-id c mboid)]
        (when (and (= command "setflag") (flag-read-only? (nth flinf 0)))
          (if rownum
            (do
              (put-object-flags! c (js/parseInt rownum) {:mboflag  flinf})
              (dispatch-peers! c "update-mboflag"
                               {:control-name c :mboid mboid :rownum rownum :readonly (nth flinf 1)}))
            (put-pending-flags! c mboid (nth flinf 1)))
          )
        (when (= command "setfieldflag")
          (let [field (nth flinf 0)
                flag (nth flinf 1)
                flagval (nth flinf 2)
                local-flag (get-local-flag c rownum field)
                lro (when local-flag  (nth local-flag 0))
                lrq  (when local-flag  (nth local-flag 1))
                flag-to-send (if (flag-read-only? flag)
                               [(str->bool flagval) (if lrq lrq false)]
                               (when (flag-required? flag)
                                 [(if lro lro false) (str->bool flagval)]
                                 ))
                ]
            (when flag-to-send
              (if rownum
                (do
                  (put-object-flags-attrval! c rownum field  flag-to-send)                  
                  (dispatch-peers! c  "update-fieldflag"
                                   {:control-name c :mboid mboid :rownum rownum :field field :flag flag-to-send}))
                (put-pending-flags-attrval! c mboid field flag-to-send)))))))))


(defn dispatch-datarow [control-name rownum]
  (when-let [od (-> (@object-data control-name) :data)]
    (when-let [fields (get od rownum)]
      (doseq [column (keys fields)]
        (let [colval (get fields column)]
          (dispatch-upd control-name  rownum column colval))))))


(defn- change-the-id-map! [control-name mboid rownum]
  (swap! object-data (fn [c]
                       (-> c
                           (update-in  [control-name :idsmap]
                                       assoc (js/parseInt rownum) mboid)
                           (update-in  [control-name :rowsmap] assoc mboid (js/parseInt rownum))))))

(def curr-unique-id-promises
  (atom {})
                                        ;every container has to have the current unique id. However it is populated only when the fetch is done, so it is possible to have the control-data without uniqueid, becuse the addmbo puts also the data into the control. For storing the data in the offline, we need to know the parent unique id always, so we introduce the promise here
  )

(defn get-curr-uniqueid-promise
  [control-name rownum]
  (when-let [cp (@curr-unique-id-promises [control-name rownum])]
    (when-not (p/has-fired? cp) cp)))

(defn get-new-curr-uniqueid-promise
  [control-name rownum]
  (let [d (p/get-deferred)]
    (swap! curr-unique-id-promises assoc [control-name rownum] d)
    d))


(defn get-local-data-by-uniqueid
  [control-name uniqueid]
  (loop [i 0]
    (when-let [lc (get-local-data-all-attrs control-name i)]
      (if (= uniqueid (get (:data lc) "_uniqueid"))
        lc
        (recur (inc i))))))

(defn set-control-index [ev]
  (let [control-names (nth ev 0)
        mboid (nth ev 1)
        rownum (nth ev 2)
        ]
    (doseq [control-name control-names]
      (let [control (get-connected-control control-name)
            _mess {:mboid mboid :currrow rownum :numrows 1}]
        (when (and control (@container-registry control))
          (let [prev-row (get-state control :currrow)
                uid (get-local-data control-name rownum "_uniqueid")
                org (get-state control :uniqueid)
                mess (if uid
                       (if-not (p/has-fired? org)
                         (do
                           (p/callback org uid)
                           _mess)
                         (do
                           (assoc _mess :uniqueid (p/get-resolved-promise uid))))
                       (do
                         _mess))]
            (set-states control mess)
            (change-the-id-map! control-name mboid rownum)
            (dispatch-peers! control-name "set-control-index" {:control-name control-name :mboid mboid :currrow rownum :prevrow prev-row})
            (dispatch-datarow control-name rownum)
            (if (not (= -1 rownum))
              (dispatch-peers! control-name "setting-done" {:control-name control-name}))))))))

(defn clear-data-cache [control-name]
  ;;when clearing the data preserve the meta
  (let [metadata (:metadata (@object-data control-name))]
    (swap! pending-data #(dissoc % control-name))
    (swap! object-data (fn [o]
                         (->
                          (dissoc o control-name)
                          (assoc-in [control-name :metadata] metadata))))))

(defn clear-control-data [control-name]
  (clear-data-cache control-name))

(declare deleteOfflineData)

(declare offline-move-in-progress)

(defn reset-controls [control-names]
  (doseq [control-name control-names]
    (clear-control-data control-name)
    (when (and (not (get-state control-name :dettached))
               (is-offline-enabled control-name)
               (not @offline-move-in-progress)
               (not @is-offline)) ;must have support for offline reset, because the offlne search, but it should not delete the offline data like the regular reset
      (->
       (get-parent-uniqueid control-name)
       (p/then (fn ([puid] (deleteOfflineData control-name puid))))))
    (dispatch-peers! control-name "reset" {:data "dummy"})))

(defn command-mboset [ev]
  (let [command (nth ev 0)
        control-names (nth ev 1)]
    (when (= "resetThis" command) 
      (reset-controls control-names))))

(defn command-mbo [ev]
  (let [command (nth ev 0)
        control-names (nth ev 1)
        cid (nth ev 2)]
    (doseq [control-name control-names]
      (let [rownum (get-row-from-id control-name cid)]
        (if-let [select-val  (case command
                               "select" "Y"
                               "unselect" "N"
                               nil
                               )]
          (do
            (put-object-data-attrval! control-name rownum  "_SELECTED" select-val)
            (dispatch-upd control-name   rownum "_SELECTED" select-val))
          (case command
            "delete"
            (do
              (dispatch-peers! control-name "delete" 
                               {:control-name control-name :mboid cid :value true :rownum rownum})
              (put-object-data-attrval! control-name rownum "deleted" true))
            "undelete" (do 
                         (dispatch-peers! control-name "delete" 
                                          {:control-name control-name :mboid cid :value false :rownum rownum})
                         (put-object-data-attrval! control-name rownum "deleted" false))))))))

(defn default-ev-process [event-name event-data]
  )

(defn on-add-mbo [ev]
  (let [controls (nth ev 0)
        mbo-id (nth ev 1)
        rownum (nth ev 2)
        ]
    (doseq [c controls]
      (change-the-id-map! c mbo-id rownum)
      (move-data-and-flags-from-pending c mbo-id)
      (when (and
             (rows-in-local? c (js/parseInt rownum) 1)
             (not= (get-id-from-row c (js/parseInt rownum) ) mbo-id)
             )
        (trim-object-data c (js/parseInt  rownum))
        )
      (dispatch-peers! c "addmbo" {:rownum rownum :mboid mbo-id})
      )))


(defn ev-dispf-from-str[se]
  (let [event-name (nth se 0)
        event-data (subvec se 1)]
    (cond
      (= event-name "logout") (js/setTimeout (fn [_] (page-init)) 300)
      (= event-name "updatembovalue") (update-mbovalue event-data)
      (= event-name "commandmbo-param") (update-control-attribute-flags event-data)
      (= event-name "set-current-index") (set-control-index event-data)
      (= event-name "commandmboset") (command-mboset event-data)
      (= event-name "commandmbo") (command-mbo event-data)
      (= event-name "addmbo") (on-add-mbo event-data)
      :else      (default-ev-process event-name event-data)
      )))

(defn ev-dispf[e]
  (ev-dispf-from-str e)
  )


(defn bulk-ev-dispf [bulk-event];optimizacija performansi
  (doseq [e bulk-event]
    (try  (ev-dispf-from-str e) (catch js/Error e
                                  (if js/console
                                    (.log js/console e);;remove
                                    (u/debug e)))) 0))

(defn error-dispf[e]
  (let [error-code (nth e 0)
        error-text (nth e 1)
        status (nth e 2)]
    (if (= status 401)
                                        ;      (global-login-function e)
      (.call (aget globalFunctions "global_login_function") nil e);indirection required becuase of advanced compilation
      (if  (= status 0)
        (start-receiving-events) ;fake alarm, continue the longpoll
        (.call (aget globalFunctions "globalErrorHandler") nil "longpoll" [error-text] nil )
        ))))


(defn start-receiving-events[];treba da napravim jos dva metoda, jedan za obican poll, a drugi za web sockete. korisnik ce moci da konfigurise koji mu odgovara.
  (when-not @is-offline
    (net/start-server-push-receiving
     bulk-ev-dispf
     process-push-receiving-error)))

(defn stop-receiving-events
  []
  (when-not @is-offline
    (net/stop-server-push-receiving)))

(defn- internal-page-destructor
  []
  (loop [arr wait-actions]
    (when-not (ar/empty? arr)
      (ar/pop! arr)
      (recur arr))))

(declare get-main-containers)

(defn unregister-deferred
  [cont]
  (let [ch (.getChildren cont)]
    (remove-deferred cont)
    (doseq [c ch]
      (unregister-deferred c))))

(defn re-register-deferred
  [cont]
    (let [ch (.getChildren cont)]
    (set-deferred cont)
    (doseq [c ch]
      (re-register-deferred c))))

(def was-logging-in (atom false))

(defn ^:export page-init
  "what is common for every page to init. First thing it does is to initialize the server side components, which will check whether the user has already been logged in or not"
  []
  (reset! object-data {})
  (reset! pending-data {})
  (when-not @page-init-called
    (internal-page-destructor)
    (reset! page-init-called true))
  
  (stop-receiving-events)
  (-> (is-server-offline?)
      (p/then
       (fn [offline?]
         (if offline?
           (do
             (reset! is-offline true)
             (reset! was-offline true)
             (reset! page-opened true)
             (go (put! @page-init-channel "offline")
                 (put! @logged-in-chan true)))
           (let [page-already-opened @page-opened]
             (reset! page-opened true)
             (->
              (p/get-promise
               (fn [resolve reject]
                 (net/send-get-with-timeout
                  (net/init)
                  (fn [_ts] 

                    (net/set-tabsess! (first _ts) )
                    (go (put! @page-init-channel (first _ts))
                        (put! @logged-in-chan true))
                    
                    (start-receiving-events)
                    (resolve (first _ts))
                    (let [app-containers (get-app-containers)]
                      (if @was-offline
                        ;;it was offline, and now its offline, and it must be after login(otherwisere
                        ;;this would be error with 401)
                        (do
                          (reset! logging-in  false)
                          (reset! is-offline false)
                          (reset! was-offline false)
                          (->
                           (late-register app-containers)
                           (p/then
                            (fn [_]
                              (doseq [c app-containers]
                                (when (and (not @is-offline) (is-offline-enabled c))
                                  (post-offl-changes c
                                                     (fn [ok] (println "offline posting finished"))
                                                     (fn [err] (println "error when posting offline")))))))))
                        (do
                          (when @was-logging-in
                            (doseq [cont app-containers]
                              (unregister-deferred cont)
                              (re-register-deferred cont)
                              (doseq [c (.getChildren cont)]
                                (when-not (get-state c :iscontainer)
                                  (.onReset c))))
                            (reset! was-logging-in  false))))))
                  (fn [err]
                    (let [err-type (get err 1)
                          err-code (get err 2)]
                      (println err)
                      (println err-type)
                      (if (or
                           (= err-type "TIMEOUT")
                           (= err-type "OFFLINE")
                           (and
                            (= err-type "HTTP_ERROR")
                            (not= err-code 401)))
                        (do
                          (reset! is-offline true)
                          (reset! was-offline true)
                          (go (put! @page-init-channel "offline")
                              (put! @logged-in-chan true))
                          )
                        (do
                          (reset! page-init-called false)
                          (go (put! @page-init-channel "previous"))
                          (reset! page-init-channel (promise-chan))
                          (swap! logging-in (fn [_] true))
                          (reset! was-logging-in true)
                          (.call (aget globalFunctions "global_login_function") nil err)))
                      (resolve err)))
                  PAGE-INIT-TIMEOUT)))
              (p/then-catch
               (fn [err]
                 (reset! page-init-called false)
                 (reset! page-init-channel (promise-chan))
                 (reset! logged-in-chan (promise-chan)))))))))))


(defn get-control-metadata
  [control]
  (-> (@object-data control) :metadata))

(defn is-persistent?
  [control]
  (-> (@object-data control) :metadata second :mboPersistent))

(defcmd-with-prepare register-mboset-with-one-mbo [control-name parent-control uniqueid]
  (add-relationship control-name (aget rel-map parent-control) nil)
  (fn [evt]
    (let [resp (first evt)
          already-reg (first resp)
          ]
      (add-peer-control nil control-name))))

(defn offline-register-mboset-with-one-mbo [control-name parent-control uniqueid cb errb]
  (add-relationship control-name (aget rel-map parent-control) nil)
  (add-peer-control nil control-name)
  (when cb (cb "done")))



(offline-alt-noobj register-mboset-with-one-mbo-with-offline
                      register-mboset-with-one-mbo
                      offline-register-mboset-with-one-mbo
                      [control-name parent-control uniqueid]
                      )

(defcmd register-mboset-with-one-mbo-ind [control-name parent-control parent-index]
  (fn [evt]
    (let [resp (first evt)
          already-reg (first resp)
          ]
      (add-peer-control nil control-name)
      ))
  )

(defcmd-with-prepare register-mboset-byrel [control-name rel-name parent-control]
  (add-relationship control-name rel-name parent-control)
  (fn [evt]
    (let [resp (first evt)
          already-reg (first resp)]
      (if (= "none" already-reg)
        (add-peer-control nil control-name)
        (add-peer-control already-reg control-name)))))

(defn offline-register-mboset-byrel [control-name rel-name parent-control cb errb]
  (add-relationship control-name rel-name parent-control)
  (add-peer-control nil control-name)
  (when cb (cb "done")))

(offline-alt-noobj register-mboset-byrel-with-offline register-mboset-byrel offline-register-mboset-byrel [control-name rel-name parent-control])

(defcmd register-mbo-command [control-name parent-control command arg-control]
  (fn [evt]
    (let [resp (first evt)
          already-reg (first resp)
          ]
      (if (= "none" already-reg)
        (add-peer-control nil control-name)
        (add-peer-control already-reg control-name)))))

(defcmd register-mboset-command [control-name parent-control command arg-control]
  (fn [evt]
    (let [resp (first evt)
          already-reg (first resp)
          ]
      (if (= "none" already-reg)
        (add-peer-control nil control-name)
        (add-peer-control already-reg control-name)))))

(def wf-directors (atom {}))


(defcmd register-wf-director [control-name app-name process-name director-name]
  (fn [evt]
    (swap! wf-directors assoc director-name [control-name process-name app-name])))

(defn offline-register-wf-director [control-name app-name process-name director-name cb errb]
  (swap! wf-directors assoc director-name [control-name process-name app-name])
  (when cb (cb "ok"))
  )

(offline-alt-noobj register-wf-director-with-offline register-wf-director offline-register-wf-director
                      [control-name app-name process-name director-name])

(defcmd unregister-wf-director [director-name]
  (fn [evt] (swap! wf-directors dissoc director-name)))

(defn offline-unregister-wf-director [director-name cb errb]
  (swap! wf-directors dissoc director-name)
  (when cb (cb "ok"))
  )

(offline-alt-noobj unregister-wf-director-with-offline unregister-wf-director offline-unregister-wf-director [director-name])



(defcmd route-wf [actionsset-name control-name app-name director-name ])

(defn offline-route-wf [actionset-name control-name app-name director-name cb errb]
  (->
   (offline/routeWF (aget rel-map control-name) (get-currow control-name))
   (p/then (fn [rez]
             (let [nac (-> rez (aget 0) (aget "actions"))]
               (when-not (= nac "empty")
                 (add-relationship actionset-name (aget nac 1) nil)
                 (add-peer-control nil actionset-name)
                 )
               (when cb (cb rez))
               rez
               )))
   (p/then-catch
    (fn [err] (when errb (errb err)) err))))

(offline-alt-noobj route-wf-with-offline route-wf offline-route-wf [actionset-name control-name app-name director-name])



(defcmd choose-wf-actions [actionsset-name director-name])

(defn offline-choose-wf-actions [actionsset-name director-name cb errb]
  (let [control-name (first (@wf-directors director-name))
        table-name (aget rel-map control-name)
        rownum (get-currow control-name)]
    (->
     (offline/chooseWFAction table-name rownum)
     (p/then (fn [rez]
               (let [nac (-> rez (aget 0) (aget "actions"))]
                 (when-not (= nac "empty")
                   (add-relationship actionsset-name (aget nac 1) nil)
                   (add-peer-control nil actionsset-name)
                   )
                 (when cb (cb rez))
                 rez
                 )))
     (p/then-catch (fn [err] (when errb (errb err)) err)))))

(offline-alt-noobj choose-wf-actions-with-offline choose-wf-actions offline-choose-wf-actions [actionset-name director-name])

(defcmd initiate-wf [actionsset-name control-name app-name director-name ]
  (fn [evt]
    (let [resp (first evt)
          already-reg (first resp)
          ]
      (if (= "none" already-reg)
        (add-peer-control nil actionsset-name)
        (add-peer-control already-reg actionsset-name))))
  )

(defcmd reassign-wf [actionsset-name director-name])

(defcmd execute-reassign-wf [actionsset-name director-name]
  (fn [evt]
    (let [resp (first evt)
          already-reg (first resp)
          ]
      (if (= "none" already-reg)
        (add-peer-control nil actionsset-name)
        (add-peer-control already-reg actionsset-name)))))

(defcmd cancel-wf [director-name])

(defcmd is-active-wf [control-name])

(defn offline-is-active-wf [control-name cb errb]
  (cb true)
  true
  )

(offline-alt-noobj is-active-wf-with-offline is-active-wf offline-is-active-wf [control-name])

(defcmd prefetch-wf-for-offline
                                        ;this one doesn't use the wf director becuse it would actually start the workflow
  [control-name process-name]
  )

                                        ;promises synchronization is done on controls level, this is why we need separate function, because the control has fist to get its promise, and then to insert data into offline
(defn insert-prefetch-offline
  [control-name uniqueid evt]
  (let  [_dta (aget evt 0)
         mta (aget _dta 0)
         dta (aget _dta 1)
         table-name (aget rel-map control-name )]
    (->
     (offline/insertCoreWFMeta mta)
     (p/then (fn [_]
               (offline/insert-offline-wf-with-id table-name uniqueid dta))))))

(defcmd-post offline-replay-wf [control-name app-name process-name wf-steps])


(defcmd unregister-control [control-name]
  (fn [evt]
    (swap! object-data #(dissoc % control-name))))

(defn offline-unregister-control 
  "the correct implementation would queue the control for the de-regitering on the server side after it againg goes online. However, the drawback is very minimal. This also should NOT delete the offline data, because we may want to register controls while offline, and reuse the offline database"
  [control-name rel-name cb errb]
  (swap! object-data #(dissoc % control-name))
  (when cb (cb "ok")))

(offline-alt unregister-control-with-offline unregister-control offline-unregister-control [control-name])

(declare re-register-mboset-byrel-with-offline)

(defcmd re-register-mboset-byrel
  [control-name rel-name parent-control]
  (fn [evt]
    (let [resp (first evt)
          already-reg (first resp)
          ]
      (clear-control-data control-name)
      (if (= "none" already-reg)
        (add-peer-control nil control-name)
        (add-peer-control already-reg control-name)))))

(defcmd re-register-mboset-with-one-mbo
  [control-name parent-control parent-id]
  (fn [evt]
    (let [resp (first evt)
          already-reg (first resp)
          ]
      (clear-control-data control-name)
      (if (= "none" already-reg)
        (add-peer-control nil control-name)
        (add-peer-control already-reg control-name)))))

(defn re-register-mboset-byrel-and-fetch
  "isto kao gore, samo jos i fetchuje posle toga"
  [control-name rel-name parent-control]
  (re-register-mboset-byrel-with-offline control-name rel-name parent-control
                                         (fn [e]
                                           (fetch control-name)
                                           )))

(defcmd register-query-mboset
  [control-name app]
  (fn [evt]
    (let [resp (first evt)
          already-reg (first resp)
          ]
      (add-peer-control nil control-name)
      )))


(defcmd-with-prepare register-inbox-mboset
  [control-name]
  (add-relationship control-name "wfassignment" nil);just for the offline, i have yet to see how it will work once it is offline
  (fn [evt]
    (let [resp (first evt)
          already-reg (first resp)
          ]
      (add-peer-control nil control-name)
      )))

(defcmd-with-prepare register-person-mboset
  [control-name]
  (add-relationship control-name "person" nil)
  (fn [evt]
    (let [resp (first evt)
          already-reg (first resp)]
      (add-peer-control nil control-name)
;just for the offline, i have yet to see how it will work once it is offline
      )))

                                        ;here we have the same logic like in register-mainset. ALl the other dependent containers depend on mboocontainer, and for them page-init-deferred had always been already fired. InboxContainer doesn't depend on AppContainer
(defn ^:export register-inbox
  [control-name cb errb]
  (p-deferred-on @page-init-channel
          (register-inbox-mboset control-name cb errb)))

(defcmd register-bookmark-mboset
  [control-name app]
  (fn [evt]
    (let [resp (first evt)
          already-reg (first resp)]
      (add-peer-control nil control-name))))

(defcmd use-stored-query
  [control-name query-name])


(defcmd register-gl-format ;istovremeno registruje format i mboset
  [glname orgid]
  (fn [evt]
    (add-peer-control nil glname)))

(defcmd set-segment 
  [glname segment-values segment-no orgid])

(defcmd post-yes-no-cancel-input 
  [ex-id user-input])

(defcmd remove-yes-no-cancel-input 
  [ex-id])

(defn simulate-error-response
  "used to simulate the ajax error from the javascript error, so the error handlers work properly"
  [message]
  (println "error-response " message)
  #js [["js" message "browser" "javascript"] 6 500]
  )

(defn offline-error-response
  []
  (simulate-error-response "Not available while offline")
  )

(def ^:export YES 8)
(def ^:export NO 16)
(def ^:export CANCEL 4)

                                        ;(defn ^:export yesnocancelErrorHandler
                                        ;  [ex-message ex-group ex-key proxyf]
                                        ;  (let [dial (goog.ui.Dialog.)
                                        ;        ]
                                        ;    (.setTitle dial "Choose Yes, No or Cancel")
                                        ;    (.setContent dial ex-message)
                                        ;    (.setButtonSet dial goog.ui.Dialog.ButtonSet/YES_NO_CANCEL)
                                        ;    (.setVisible dial true)
                                        ;    (globalRemoveWaitCursor)
                                        ;    (goog.events/listen 
                                        ;     dial
                                        ;     goog.ui.Dialog.EventType/SELECT
                                        ;     (fn [e]
                                        ;       (globalDisplayWaitCursor)
                                        ;       (proxyf
                                        ;        (let [key (.-key e)]
                                        ;          (case key
                                        ;            "yes" YES
                                        ;            "no" NO
                                        ;            CANCEL)))))))

                                        ;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                        ;TODO I have removed this to get rid of goog.ui. This should definitely be configured with some default settings for all the settings. Furterhmore, we have to put this in globalFunctions to be customizable
(defn ^:export yesnocancelErrorHandler
  [ex-message ex-group ex-key proxyf]
  (js/alert (str "!Yes no cancel error handler not configured. Error message:" ex-message))
  )


(defn yesnocancelproxy
  [data okf errf post?]
  (let [proxy-error
        (fn [err]
          (if-let [err-arr (and (vector? err) (aget err 0)) ]
            (let [error-code (aget err-arr 0)]
              (if (= error-code "yesnocancel")
                (let [ex-id (aget err-arr 1)
                      ex-message (aget err-arr 2)
                      ex-group (aget err-arr 3)
                      ex-key (aget err-arr 4)
                      fnproxy (fn [user-input]
                                (post-yes-no-cancel-input 
                                 ex-id user-input
                                 (fn [ok]
                                   (yesnocancelproxy data
                                                     (fn [_ok]
                                                       (remove-yes-no-cancel-input ex-id
                                                                                   (fn [_] (okf _ok))))
                                                     (fn [_err]
                                                       (remove-yes-no-cancel-input ex-id
                                                                                   (fn [_] (errf _err))))
                                                     post?))))]
                  (yesnocancelErrorHandler ex-message ex-group ex-key fnproxy))
                (errf err)))
            (errf err)))]
    (if @is-offline
      (errf (offline-error-response))
      (do
        (when-not @page-init-called
          (page-init))
        (p-deferred-on @page-init-channel
                       (if post?
                         (net/send-post (net/command) data okf proxy-error)
                         (net/send-get (str (net/command) "?" data) okf proxy-error)))))))

                                        ;helper function for macro, to reduce the generated file size

(defn- get-new-promise
  [container command f]

  (let [promise  (p/get-promise f)]
    (aset promise "container" container)
    (aset promise "command" command)
    promise))


(defn ^:export offlineExists
  "Check if the offline has been prepared or not. User may want to keep the tables, or decide to re-create everything from the scratch"
  []

  )

(defn ^:export offlineReset
  "Re-create all the tables"
  []
  )


(defn ^:export offlinePrepare
  "Gets all the persistent objects metadata and re-create the tables"
  []
  )

(defn ^:export offlinePrepareOne
  "Lists will not be prepared with the offlinePrepare. User will have the option to run the manual load of the value lists"
  [object-name]
  (let [rel-name (aget rel-map object-name)]
    (->
     (offlineMetaMove rel-name (-> object-name get-control-metadata ))
     (p/then (fn [ok]
               (offline/bulk-update-data-and-flags rel-name (@object-data object-name))
               )))))

(defn ^:export offlineMetaMove
  [relName meta]
  (offline/moveMeta relName meta)
  )

(defn offline-insert-qbe
  "qbe is read by the tables, we just need to put something on the front end"
  [object-name qbe]
  (let [rel-name (aget rel-map object-name)]
    (offline/insert-qbe rel-name qbe)))

(defn get-first-non-single
  [containerid]
  (if-not
      (and  containerid
          (get-state containerid :singlembo))
    containerid
    (when-let [parent-id (get-state containerid :parentid)]
      (if-not (get-state parent-id :singlembo)
        parent-id
        (get-first-non-single parent-id)))))

(defn- get-parent-uniqueid
  [containerid]
  (let [table-name (aget rel-map containerid)
        list?  (.startsWith table-name "list_")
        single-mbo? (get-state containerid :singlembo)
        parent-id (if single-mbo?
                    (when-let [first-nonsingle (get-first-non-single containerid)]
                      (get-state first-nonsingle :parentid))
                    (when containerid
                      (get-first-non-single
                       (get-state containerid :parentid))))]
    (if list?
      (p/get-resolved-promise -1)
      (if parent-id
        (let [currrow-parent (get-state parent-id :currrow)
              parent-uniqueid (get-local-data parent-id currrow-parent "_uniqueid")]
          (p/get-resolved-promise parent-uniqueid))
        (p/get-resolved-promise nil)))))


(defn offline-table-count
  [containerid object-name cb errb]
  (->
   (get-parent-uniqueid containerid)
   (p/then
    (fn [parentid]
      (offline/table-count object-name parentid)))
   (p/then
    (fn [cnt]
      (when cb (cb #js [cnt]))))
   (p/then-catch
    (fn [err]
      (when errb (errb err))))))

(defn offline-get-qbe [container-id object-name cb errb]
  (->
   (offline/get-qbe object-name)
   (p/then
    (fn [qbe]
      (when cb (cb qbe))))
   (p/then-catch
    (fn [err]
      (when errb (errb err))))))

(defn offline-re-register-mboset-byrel
  [container-id object-name rel-name parent-control cb errb]
  (clear-control-data container-id)
  (when cb (cb "ok"))
  (p/get-resolved-promise "ok")
  )

(defn offline-re-register-mboset-with-one-mbo
  [container-id  object-name parent-control parent-id cb errb]
  (clear-control-data container-id)
  (when cb (cb "ok"))
  (p/get-resolved-promise "ok")
  )

(offline-alt re-register-mboset-byrel-with-offline
                re-register-mboset-byrel
                offline-re-register-mboset-byrel
                [container-id rel-name parent-control])

(offline-alt re-register-mboset-with-one-mbo-with-offline
                re-register-mboset-with-one-mbo
                offline-re-register-mboset-with-one-mbo
                [container-id parent-control parent-id])

(offline-alt mboset-count-with-offline mboset-count offline-table-count [containerid])

(offline-alt get-qbe-with-offline get-qbe offline-get-qbe [containerid])

(defn offline-move-to 
  "we assume move-to will come from controls so here just the echo plus the event dispatch will be done"
  [control-name rel-name rownum cb errb]
  (->
   (get-parent-uniqueid control-name)
   (p/then (fn [parent-id] (db/get-unique-id rel-name parent-id rownum)))
   (p/then
    (fn [uniqueid]
      (toggle-state control-name :uniqueid (p/get-resolved-promise uniqueid))
      (toggle-state control-name :currrow rownum)
      (dispatch-peers! control-name "set-control-index"
                       {:control-name control-name :currrow rownum})
      (when cb (cb [rownum]))))
   (p/then-catch (fn [err] (when errb (errb err)) err))))

(offline-alt fetch-multi-rows-with-offline fetch-multi-rows fetch-multi-rows-offline [control-name start-row num-rows])

(offline-alt fetch-multi-rows-with-offline-no-reset fetch-multi-rows-no-reset fetch-multi-rows-offline [control-name start-row num-rows])

(offline-alt move-to-with-offline move-to offline-move-to [control-name rownum])

(defn get-ids-for-parent [control-name parent-id & raw?]
  "implementation should return promise"
  (if parent-id
    (offline/get-ids-for-parent (aget rel-map control-name) parent-id (first raw?))
    (p/get-resolved-promise [nil]));if parent id is null that means delete the whole table. we return vector with null, so this null will be propagated to children
  )

(defn delete-for-parent [control-name parent-id & raw?]
  "implementation should return promise"
  (let [table-name  (aget rel-map control-name)]
    (when-not (get-state control-name :singlembo)
      (offline/delete-for-parent table-name parent-id (first raw?)))))


                                        ;offline delete should be atomic, so recursive delete should not be done here, instead just prepare the hierarchy of control names to be deleted and call the atomic function in offline.cljs
(defn prepare-offline-delete
  [control-name parentid]
  
  )

(def offline-move-in-progress (atom false))
                                        ;this flag should prevent deletion of the offline when reset during the full offloading. 
(defn set-offline-move-in-progress
  [flag]
  (swap! offline-move-in-progress (fn [_] flag)))

(defn get-offline-move-in-porgress
  []
  @offline-move-in-progress
  )

(defn ^:export deleteOfflineData
  [control-name & parentid]
  (delete-for-parent control-name (first parentid) :raw)
  )



(defn get-main-containers
  [& condition]
  (loop [ks  (keys @container-registry) rez []]
    (if (empty? ks)
      rez

      (recur (rest ks) 
             (let [cid (first ks)]
               (if (and (get-state cid :mainmboset) (if condition ((first condition) cid) true))
                 (conj rez (@container-registry cid))
                 rez))))))

(defn get-app-containers
  []
  (filter some?
          (map (fn [[k v]]
                 (when (get-state k :appcont) v))
               @container-registry)))

(defn one-cont-late-register
  [cont]
  (let [deferred (promise-chan)
        registered-columns (@registered-columns (get-id cont))]
     (u/debug "1.calling conta late register init for " (get-id cont))
    (set-states cont {:deferred deferred})
    (->
     (cont-late-register cont)
     (p/then
      (fn [_]
        (u/debug "2.calling conta late register init for " (get-id cont))
        (p! cont "registercol" add-control-columns registered-columns)))
     (p/then
      (fn [_]
        (u/debug "3.calling conta late register init for " (get-id cont))
        (go (put! deferred "finished"))
        (p/get-resolved-promise "registered")))
     (p/then
      (fn [_]
        (u/debug "calling conta late register init for " (get-id cont))
        (clear-data-cache (get-id cont))
        (cont-late-register-init cont)
;;        (if (get-state cont :parentid);;that is if its rel or dependent container
;;          (p/get-resolved-promise "rel-container")
;;          (cont-late-register-init cont))
        )))))

(defn late-register 
  [containers] ;;relcontainers, just register don't reset
  (if (empty? containers)
    (p/get-resolved-promise "empty");already registered, from online->offline and then back
    (p/prom-all
     (doall
      (map (fn [c]
             (->
              (one-cont-late-register c)
              (p/then
               (fn [_]
                 (when-let [ch-rels (.getRelContainers c)]
                   (late-register ch-rels))))))
           containers)))))


;;(defn register-controls-post-login
;;cont has to be tested and thought about more. Currently all login methods in demos either open the login page and destroy the current page, or destroy it manually. This one will agai call everything. The question is what happens with the data that was changed offline in this case"
;;  []
;;  (-> get-main-containers late-register)
;;;;
;;  )


                                        ;replay the offline workflow if the steps are finished for all the finished offline workflows. For the currently active record it doesn't have to be finished, we can continue
(defn replay-wf-from-offline
  [app-container app-name process-name finished? cb errb]
  (let [rel-name (aget rel-map app-container)
        unique-id (get-local-data app-container (get-state app-container :currrow) "_uniqueid")
        ]
    (->
     (offline/getCompletedWFActions rel-name finished? unique-id)
     (p/then (fn [actions]
               (if-not (empty? actions)
                 (offline-replay-wf app-container app-name process-name actions
                                    (fn [ok] (if finished? 
                                               (p/then (offline/deleteCompletedWFAction rel-name unique-id) (fn [_] (cb ok)))
                                               (cb ok)))
                                    (fn [err]
                                      (if (and (not @is-offline) finished?)
                                        (p/then (offline/deleteCompletedWFAction rel-name unique-id) (fn [_] (errb err)))
                                        (errb err))))
                 (errb "No finished offline workflow steps for container"))))
     (p/then-catch (fn [err] (when errb err) err)))))

(defn get-finished-offline-wfs
  [control-name]
  (offline/getFinishedOfflineWFs (aget rel-map control-name))
  )

(defcmd-post post-offline-changes [control-name data])

(defcmd save-offline-changes [control-name])

(defcmd rollback-offline-changes [control-name])

(defcmd move-to-uniqueid [control-name uniqueid])

(defn display-offline-post-error
  [errors]
  (println "Offline Post Errors:"))

(def ^:export globalFunctions
  #js{"globalDisplayWaitCursor" globalDisplayWaitCursor
      "globalRemoveWaitCursor" globalRemoveWaitCursor
      "globalErrorHandler" globalErrorHandler
      "globalCallbackHandler" globalCallbackHandler
      "globalPrepareCallHandler" globalPrepareCallHandler
      "globalFinishCallHandler" globalFinishCallHandler
      "global_login_function" global-login-function
      "handleErrorMessage" handleErrorMessage
      "globalOfflinePostError" display-offline-post-error
      "forceLongPoll" false
      "startedOffline" (fn [] (p/get-resolved-promise false))
      "notifyOffline" (fn [flag] (u/debug "Offline " flag))
      }
  );one level of indirection required so the functions can be overriden when they are compiled with the advanced compilation


(defn ^:export setGlobalFunction
  [key f]
  (aset globalFunctions key f))


(defn empty-data-tree?
  [tree]
  ;; this is helper for offline. tree has :children and :data. If all the data nodes are empty, the tree is empty
  (and (empty? (:data tree))
       (if-let  [ch-tree (:children tree)]
         (reduce
          (fn [a b]
            (and a b))
          (map empty-data-tree? ch-tree))
         true)))

(defcmd-post get-server-offline-database [schema])

(defn ^:export setDisplayWaitCursor
  [f]
  (setGlobalFunction "globalDisplayWaitCursor" f))

(defn ^:export setRemoveWaitCursor
  [f]
  (setGlobalFunction "globalRemoveWaitCursor" f))

(defn ^:export setErrorHandler
  [f]
  (setGlobalFunction "globalErrorHandler" f))

(defn ^:export setCallbackHandler
  [f]
  (setGlobalFunction "globalCallbackHandler" f))

(defn ^:export setPrepareCallHandler
  [f]
  (setGlobalFunction "globalPrepareCallHandler" f))

(defn ^:export setFinishCallHandler
  [f]
  (setGlobalFunction "globalFinishCallHandler" f))

(defn ^:export setOnLoggedOff
  [f]
  (setGlobalFunction "global_login_function" f))

(defn ^:export setErrorMessageHandler
  [f]
  (setGlobalFunction "handleErrorMessage" f))

(defn ^:export setOfflineNotifier
  [f]
  (setGlobalFunction "notifyOffline" f))
