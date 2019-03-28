(ns maximoplus.basecontrols
  (:import [goog.ui IdGenerator])
  (:require [maximoplus.utils :as u]
            [maximoplus.promises :as p]
            [maximoplus.arrays :as ar]
            [maximoplus.offline :as off]
            [clojure.string :refer [trim split]]
            [clojure.walk :as walk :refer [prewalk]]
            [maximoplus.core :as c :refer [Receivable Component Offline]]
            [cljs.core.async :as a :refer [put! <! >! chan buffer poll! promise-chan]])
  (:require-macros [maximoplus.macros :as mm :refer [def-comp googbase kk! kk-nocb! kk-branch-nocb! p-deferred p-deferred-on custom-this kc! kk-branch!]]
                   [cljs.core.async.macros :refer [go go-loop]])
  )

(def id-generator (IdGenerator.))

(defn get-next-unique-id [& prefix]
  (str (.getNextUniqueId id-generator) (when prefix (first prefix)))
  )

(defprotocol Foundation
  (get-prefix [this]);for constructing the id
  (add-child [this child])
  (remove-child [this child])
  (get-children [this])
  (get-parent [this])
  (dispose [this])
  (add-virtual-column [this ind column metadata])
  (set-max-value [this column value])
  (add-meta [this column metaKey metaValue])
  (dispose-child [this child]);some libraries don't allow referencing from child to parent(React), so if we need to manipulate parent state after the row is deleted we require one level of indirection like this
  (add-wrapped-component [this wrapped]);;for the react, vue and skatejs  frameworks we need a two-way handler
  )

(defprotocol MessageProcess
  (on-fetched-row [control x])
  (on-reset [control])
  (on-set-control-index [control row])
  (on-trimmed [control row])
  (on-add-at-index [control row])
  (on-set-fieldflag [control row field flag])
  (on-update-control-data [control rownum column value])
  (on-delete [control rownum value])
  (on-fetch-finished [control]))



(defprotocol App
  (mbo-command [this command argControl callback errback])
  (mboset-command [this command argControl callback errback])
  (callback-handler [this command  ev-target]);ev-target je tamo gde sam kliknuo pre akcije
  (errback-handler [this error-message error-code error-group error-key])
  (prepare-call [this command ev-target])
  (finish-call [this command ev-target])
  (access-to-option [this option callback errback])
  (get-app [this])
  )

(defprotocol Container
  (get-rel-containers [this])
  (re-register-and-reset [cont cb errb]) ;previously the reset for the relcontainer also re-register it, which is bad if we manually want to reset the container after the qbe was set. Re-registration is required if the parent index has been changed
  (get-key-attributes [this cb errb])
  (fetch-data [this start numrows cb errb])
  (fetch-current [this cb errb])
  (init-data-with-off [this start numrows cb errb])
  (fetch [this cb errb])
  (reset [this cb errb])
  (add-new-row [this callback errback])
  (add-new-row-at [this rownum callback errback])
  (move-to-row [this row-num cb errb])
  (move-next [this cb errb])
  (move-prev [this cb errb])
  (del-row [this callback errback])
  (undel-row [this callback errback])
  (get-currow [this])
  (get-local-data [this rownum])
  (save [this cb errb])
  (get-qbe [this cb errb])
  (set-qbe [this column value cb errb])
  (set-order-by [this column  cb errb])
  (set-value [this column value cb errb])
  (get-row-count [this cb errb])
  (get-field-local-value [this column] )
  (use-stored-query [this queryName])
  (after-fetch [this cb]);promise executed on fetch
  (register-columns [this columns cb errb])
  (deregister-columns [this columns])
  (late-register [control]);if the container was created offline and it goes online it needs to register on the server. Same goes if the session is expired during offline, and our login method keeps us on page without re-registering everything again
  (get-offline-objects-tree [control])
  (get-offline-changes [control])
  (post-offline-changes [control cb errb])
  (save-offline-changes [control cb errb])
  (rollback-offline-changes [control cb errb])
  (offline-post-finished [control res])
  (move-to-uniqueid [control uniqueid cb errb])
  (get-local-data-by-uniqueid [control uniqueid])
  (get-unique-id [container]);just for unique containers
  )

(defprotocol UI
  (on-set-deleted [control deleted])
  (clear-control [control])
  (on-set-readonly [control flag])
  (on-set-max-value [control column value])
  (set-enabled [control enable])
  (set-readonly [control flag])
  (set-required [control flag])
  (is-enabled [control])
  (render [cotrol]) ;this should crate the visual part of the component and attach it to the parent control . In general case it is not possible always to control how the visual part of the component will be attached to the parent, and we will not separate that here. However, for the HTML DOM compnents it will be done with the familiar create-component-dom and another optional method to attach the DOM
  (render-deferred [control]) ;render just when the deferred was fired (usually when the columns for the control have been registered)
  (on-render [control]) ; this is just like enterdocument in 1.0, after the render is done, actions which require to be done after component become visible are here
  (on-render-internal [control]);same like on-render, but not to be extended by the user
  (add-ui-listeners [control keyvals])
  (add-ui-listener [control key f])
  (listen-action [control f])
  (get-ui-event-value [control key ev]) ;differs from implementation
  (draw-section [control]) ;draws section skeleton
  (draw-row [control]);draw row skeleton
  (draw-fields [control]);one level of indirection, in rendering the row, so in case of list we don't render the fields
  (draw-field [control]) ;draw field skeleton
  (get-insertion-point [control]);in case we have the conrtol over where children are rendered (in HTML5), the visual element where the children are rendered
  (add-rendered-child [control rendered-child child]);adding the rendered child to the parent insertion point (not the element itself, its visual part). This also will not be available for Native, where we don't have control where children are added(that is defined in the native definition of the component).For HTML5 renderd-child is the DOM
  (set-visible [control visible])
  (is-visible [control])
  (render-row [control row])
  (render-row-before [control row existing-row])
  (mark-as-displayed [control]);if we have external rendering (like in react), we need to notify maximoplus that the control is displayed, so it can attach listeneres for example
  )

(defprotocol Field
  (set-focus [control])
  (local-value [control])
  (add-action [control listenF actF deferred])
  (remove-action [control removalF])
  (add-lookup-internal [control])
  (show-lookup [control])
  (add-lookup [control])
  (show-smartfill-list [control smartfill-container])
  (get-list-columns [control])
  (get-list-labels [control])
  (show-ld-lookup [control])
  (display-label [control label])
  (set-label [control label])
  (show-date-lookup [control])
  (show-date-time-lookup [control])
  (set-flag [control vals])
  (change-maximo-value [control value])
  (get-list-dialog [conrol list-container columns])
  (set-field-value [control value])
  (get-column [control])
  (show-list [control columns])
  (replace-from-local [control])
  (changed-row [control row]);use this to notify the picker when the row was changed, so inline picker list are triggered
  (set-ld-value [control value])
  (show-gl-lookup [control orgid])
  (get-metadata [control]);;now the metadata is the clojurescript object. If we need to access it from javascript, we need a function converting it
  )

(defprotocol Row
  (skip-select-action [control skip])
  (get-field [control col-name])
  (create-field [control control-metadata])
  (get-fileds [control columnz])
  (add-default-lookups [control columnz])
  (set-col-label [control column label])
  (add-field-transform [control column transform])
  (remove-field-transform [control column])
  (add-field-action [control column listen-f action-f])
  (remove-field-action [control column listen-f removal-f])
  (get-fields [control columns])
  (select-row [control selected])
  (is-deleted [control])
  (highlight-selected-row [control])
  (unhighlight-selected-row [control])
  (listen-row [control selected-actionF])
  (selected-action [control])
  (set-row-flags [control colflags])
  (set-field-flag [control field flag])
  (mark-row-as-selected [control selected])
  (get-maximo-row [control])
  (get-disp-row [control])
  (set-disprow! [control dr])
  (set-row-value [control column value])
  (set-row-values [control colvals])
  (is-focused [control])
  (set-row-field-value [control field value])
  (set-field-label [control field label])
  (row-changed [control field])
  (set-field-enabled [control field enabled?])
  (set-field-required [control field required?])
  (add-field-ui-listeners [control field listeners-map])
  (set-field-focus [control field])
  (get-columns [control]);;most of the time it is just what has been passed in constructor. However, we also can have virtual fields(not bound directly to the attributeName)
  );this will be used both for rows and sections

(defprotocol Qbe
  (init-qbe-values [control])
  (clear-qbe [control])
  (add-prepend-columns [control virtual-name qbe-prepend attribute-name title position]))

(defprotocol Table
  (after-move [control f])
  (get-data-rows [control])
  (get-data-row [control rownum])
  (remove-mboset-count [control])
  (main-grid-frame [control])
  (header-row [control])
  (grid-toolbar [control])
  (qbe-row [control])
  (qbe-row-internal [control])
  (table-rows-count [control])
  (update-paginator [this fromRow toRow numRows])
  (clear-data-rows [control])
  (get-row-control [control mxrow disprow])
  (build-row [control rowcontrol])
  (append-row [control rowcontrol])
  (get-qbe-row [control])
  (get-label-row [control])
  (get-new-addrow-num [control])
  (get-numrows [control])
  (get-firstmaxrow [control])
  (get-maxrow [control disprow])
  (get-lastrow [control])
  (get-focused-row [control])
  (set-row-focus [control row])
  (del-tab-row [control row])
  (displayed-rows [control])
  (page-next [control])
  (page-prev [control])
  (set-numrows [control numrows])
  (add-new-row-to-control [control])
  (move-control-to-row [control rownum])
  (move-control-next [control])
  (move-control-prev [control])
  (shift-to [control rownum])
  (fetch-more [control numrows])
  ;;React module will not pass the state to the children, grid and section will hold the state. Below functions are one level indirection and will be overriden by react module(normally the value is set by the row or the field, but not in react)
  (set-grid-row-values-internal [control row values])
  (set-grid-row-values [control row values])
  (set-grid-row-flags [control row flags])
  (mark-grid-row-as-selected [control row selected?])
  (highlight-grid-row [control row])
  (unhighlight-grid-row [control row])
  (set-row-visible [control row visible?])
  (add-grid-field-action [control row colulmn listenF actionF deferred])
  (remove-grid-field-action [control row column listenF removalF])
  (set-grid-row-value [control row column value])
  (row-selected-action [control row-control]); perform the action when the row is selected by the user
  (set-selectableF [control selectableF])
  (on-set-grid-row-readonly [control row dval])
  (on-set-grid-row-deleted [control row deleted?])
  )


(defprotocol ControlData
  (set-flags-from-local [control])
  (add-row [control data])
  (init-data-from [control start-row])
  (init-data-from-nd [control start-row])
  (init-data [control])
  )

(defprotocol Dialog
  (add-dialog-button [dialog button])
  (remove-dialog-button [buttonId])
  (set-title [dialog title])
  (default-action [dialog])
  (close-action [dialog])
  (get-parent-field [dialog])
  (draw-dialog [control])
  (get-selectable-grid [control  listcontainer dialogcols selectF])
  (close-list-dialog [control]);close when the selection is done
  (draw-grid-in-dialog [control listContainer listGrid])
  (cancel [control]);close if no selection was done
  ;;one level of indirection, useful for react. In React Native the navigator component has to be passed down the components. Here we delegate the display of the dialog to Section or the Grid, which are top level components, so if we need to display the dialog in the separate scene, we don't need to pass the Navigator to fields.
  (get-field-list-dialog [control field list-container dialog-cols])
  )

(defprotocol Picker
  (display-picker-list-internal [control])
  (build-picker-list [control column listcon pickerkeycol pickercol norows  selectableF])
  (get-picker-list [control])
  (pick [control]);for the row contron
  (unpick [control])
  (set-row-picker-value [control  value])
  (user-pick [control row])
  (display-picker-header [control metadata])
  (add-picker-list [control picker-list])
  (add-picker-list-internal [control picker-list]);;indirection for customization
  (pick-row [control row]); for the table control
  (unpick-row [control row]); for the table control
  (destroy-picker-list [control])
  )


(defprotocol Workflow
  (cancel-wf [control])
  (set-wf-active [contol flag])
  (set-warnings [control warnings body title])
  (route-wf [control])
  (choose-wf-action [control])
  (draw-wf-input-actions [control commandContainer])
  (draw-wf-input-actions-internal [control commandContainer])
  (reassign-wf [control])
  (execute-reassign-wf [control])
  (reassign-lookup-columns [control])
  (handle-interaction [control nextApp nextTab nextAction])
  (get-wf-input-fields [control objectName])
  (get-wf-section [control wfcont objectName secFields])
  (add-wf-action [control  f label key])
  (add-wf-memo-grid [control  memocont])
  (set-wf-title [control title])
  (add-wf-section [control  section])
  (get-memo-display-container [control])
  (get-memo-grid [control memocont columns])
  (get-wf-command-container [control command args])
  (wf-finished [control])
  (prefetch-wf-for-offline [control])
  (add-wf-memo-grid-internal [control memocont])
  (init-wf-section [control section]);one level of redirection, if the section can't be returned immediately with get-wf-section (like in react)
  (init-wf-memo-grid [control memo-grid]);same thing for the memo grid
  )

(defprotocol GL
  (set-segment [control segment-no])
  (set-segment-value [control segment-no segment-value])
  (display-segments [control])
  (get-gl-value [control])
  (get-gl-dialog-holder [control chooseF])
  (clear-gl-segments [control dialog])
  (display-gl-segment [control dialog segmentNo segmentLength segmentValue segmentName segmentDelimiter active])
  (listen-segment [control segmentEl segmentNo callbackF])
  (display-gl-segments [control dialog])
  (display-gl-picker [control dialog glContainer pickerCols pickerF])
  (get-gl-picker-list [control  glContainer pickerCols pickerF])
  (get-account-placeholder [control dialog])
  (get-picker-placeholder [control dialog])
  (highlight-gl-segment [control dialog segment-no])
  (unhighlight-gl-segments [control dialog])
  (add-gl-picker-to-dialog [control dialog picker])
  )




(defn not-implemented
  []
  (throw (js/Error. "Not implemented")))

(defn fetch-reset-rels [cont cb errb]
  (doseq [relco (get-rel-containers cont)]
    (when (c/is-main-peer (c/get-id relco))
      (re-register-and-reset relco cb errb))))

(defn- add-control-column [control ind column metadata]
  (let [columns (aget control "columns")
	new-cols (-> columns (subvec 0 ind) (conj column) (concat (subvec columns ind (count columns))) vec)]
    (c/add-virtual-column-to-metadata control column  metadata)
    (aset control "columns" new-cols)))

(defn- get-first-in-hierarchy
  [control methodname]
  (loop [control control]
    (when control
      (if-let [method (aget control methodname)]
        method
        (recur (get-parent control))))
    ))

(defn- get-first-object-in-hierarchy
  [control methodname]
  (loop [control control]
    (when control
      (if-let [method (aget control methodname)]
        control
        (recur (get-parent control))))
    ))

(defn- get-main-event-receiver
  [control]
  (when  control
    (if (aget control "mainEventReceiver")
      control
      (recur (get-parent control)))))

(defn- get-prepare-call-handler
  [control]
  ;;the purpose of this is to indicate to user that the operation is starting, default is the wait cursor for the whole page, but it can be overriden on the control level
  ;; (u/debug "get prepare call handler za " (.getId control))
  (let [_cbh (get-first-in-hierarchy control "prepareCall")
        cbh (if _cbh _cbh (aget c/globalFunctions "globalPrepareCallHandler")) ]
    (fn [command]
      (c/add-prepare-action control command)
      (cbh command control))))

(defn- get-finish-call-handler
  [control]
  ;;ideja je da se pre nego sto se pozove kontrola postavi kurzor da ce korisnik da cekica. Medjutim to moze da se promeni na nivou kontrole, tako da ako se stvarno dugo ceka moze da se prikaze nesto fensi
  ;;  (u/debug "get finish call handler za " (.getId control))
  (fn [command]
    (c/remove-prepare-action control command)
    ( (if-let [cbh (get-first-in-hierarchy control "finishCall")]
        cbh
        (aget c/globalFunctions "globalFinishCallHandler"))
     command
     control)))

(defn- get-errback-handler
  [control]
  (fn [[max-err-vec goog-xhr-code http-code]]
    (let [_errh (if-let [errh (get-first-in-hierarchy control "errbackHandler")]
                  errh
                  (aget c/globalFunctions "globalErrorHandler"))]
      (if-not max-err-vec
        (.call _errh control "Network Error" "NETWORK" goog-xhr-code http-code)
        (let [[mx-error err err-group err-code] max-err-vec]
          (.call  _errh control err (name mx-error) err-group err-code))))))

(defn- get-callback-handler
  [control]
  (if-let [cbh (get-first-in-hierarchy control "callbackHandler")]
    cbh
    (fn [_])) ;;no need for global callback, makes no sense, we have global finish handler
  )

(defn ^:export initControlDataRows 
  [control noRows & forceFetch]
  (let [container (c/get-container control)
        nrs (js/parseInt noRows)
        force (first forceFetch)]
    (let [currow (get-currow container)]
      (if (or (not currow) (= -1 currow))
        (do
          (.then
           (mm/c! control  "fetch" fetch-data container  0 nrs) ;fetch already has kk!
           (fn [ok]
;;             (println "fetch finished for cobtrol " (c/get-id control))
             )
           )
          (mm/kk-control-nocb! control container "move" c/move-to-with-offline  0)))
      
      (mm/c! control  "fetch" fetch-data container  currow nrs))))

(defn get-deferred [component]
  (c/get-state component :deferred))

(def-comp BaseComponent [] js/Object
  (fn* []
       (this-as this
         (let [id (get-next-unique-id (get-prefix this))]
           (aset this "uniqueid" id)
           (aset this "children" (atom []))
           (aset this "receiving" (atom false))
           (aset this "state" (atom {}))
           (aset this "command-channel" (chan))
           (swap! c/registered-components assoc id this)
           (when-not @c/page-init-called
             (c/page-init))
           (c/start-receiving this))))
  Component
  (get-id 
   [this]
   (aget this "uniqueid"))
  (get-state
   [this key]
   (@(aget this "state") key))
  (toggle-state
   [this key value]
   (swap! (aget this "state") assoc key value))
  (remove-state
   [this key]
   (swap! (aget this "state") dissoc key))
  (set-states
   [this kvs]
   (swap! (aget this "state") merge kvs)
   )
  Foundation
  (add-meta [this column metaKey metaValue]
            (c/add-col-attr this column metaKey metaValue))
  (dispose-child
   [this child]
   (dispose child));this will be overriden in React
  (dispose
   [this]
   (doseq [c (get-children this)]
     (dispose-child this c)))
  (get-parent [this] (aget this "parent"))
  (get-prefix [this])
  (get-children [this] @(aget this "children"))
  (add-child [this child]
             (aset child "parent" this)
             (swap! (aget this "children") conj child)
             )
  (remove-child [this child]
                (dispose-child this child)
                (swap! (aget this "children") #(remove (fn [y] (= y child)) %)))
  (add-wrapped-component
   [this wrapped]
   (aset this "wrapped" wrapped)
   (aset wrapped "mp" this))
  Receivable
  (get-receive-functions
   [this]);by default nothing
  (send-command
   [this command command-f command-cb command-errb]
   (let [cch (aget this "command-channel")
         da1 (c/get-state this :deferred)
         da2 (if (or (= command "init") (not da1));;wait for deferred (usually the constructor), unless in constructor 
               @c/page-init-channel
               da1)]
     (p-deferred-on da2
                    (go (put! cch [command command-f command-cb command-errb])))))
  (start-receiving
   [this]
   (swap! (aget this "receiving") (fn [_] true))
   (let [command-channel (aget this "command-channel")]
     (go-loop []
       (let [;[command-f command-cb command-errb] (<! (a/map (fn [_ val] val) [@c/page-init-channel command-channel]));;it will wait until page is not initialized
             [command command-f command-cb command-errb] (<! command-channel)
             cb-chan (chan)]
         (command-f
          (fn [ok]
            (try
              (command-cb ok)
              (finally (put! cb-chan ok))))
          (fn [err]
            (try
              (command-errb err)
              (finally (put! cb-chan err)))))
         (<! cb-chan)
         (recur)))))
  (stop-receiving
   [this]
   (swap! (aget this "receiving") (fn [_] false))
   ) ;this should pause the component receivig the data from the long pollint
  (send-message-sync
   [this msg components]
   ;;sending directly causes the stack overflow. I will try to put it to the quueue, and from there it should be processed in async independetly
   (doseq [id (map c/get-id components)]
     (swap! c/pending-messages assoc id (conj (if-let [exs (get @c/pending-messages id)] exs []) msg)))
   ;;   (doseq [c components]
   ;;     ;;     (c/receive-message-sync c msg)
   ;;     )
   )
  (receive-message-sync
   [this msg]
   (let [type (:type msg)
         data (:data msg)]
     (when-let [rf (get (c/get-receive-functions this) type)]
       (rf data))
     (when-let [children (get-children this)]
       (c/send-message-sync this msg
                            (filter (fn [c]
                                      (and (c/get-state c :receiver) (not (c/get-state c :container)))) children)))))
  (receive-all-pending-sync
   [this]
   (doseq [msg (@c/pending-messages (c/get-id this))]
     (let [type (:type msg)
           data (:data msg)]
       (when-let [rf (get (c/get-receive-functions this) type)]
         (rf data))
       )))
  )



(def-comp MboContainer [mboname] BaseComponent
  (fn* []
       (this-as this
         (googbase this)
         (c/add-container-to-registry this)
         (let [deferred (promise-chan)]
           (c/set-states this
                         {:currrow -1
                          :uniqueid (p/get-deferred)
                          :offlineenabled false
                          :iscontainer true
                          :receiver true
                          :rel-containers []
                          ;;                        :deferred  (kk-nocb! this "init" c/register-mainset mboname)}
                          :deferred deferred ;;I will not use the promises internally anymore for the control of processing. Function will just return the promises to the user. kk! macros will send to the "command-channel". This will be the first call to the command channel, so I can safely listen for the completion
                          })
           (kk! this "init" c/register-mainset mboname
                (fn [ok]
                  (go (put! deferred ok)))
                nil)
           )))
  Component
  (get-container
   [this]
   this)
  Foundation
  (^override get-prefix [this]
   mboname)
  (dispose [this]
           (c/unregister-control-with-offline (c/get-id this)))
  MessageProcess
  (on-reset
   [this])
  Offline
  (set-offline-enabled
   [this flag]
   (c/toggle-state this :offlineenabled flag)
   (off/enable-offline))
  (set-offline-enabled-nodel
   [this flag]
   (c/toggle-state this :offlineenabled flag))
  (is-offline-enabled
   [this]
   (c/get-state this :offlineenabled))
  Container
  (get-rel-containers
   [this]
   (c/get-state this :rel-containers)
   )
  (late-register [this]
                 (kk-nocb! this "init" c/register-mainset mboname))
  (get-offline-objects-tree
   [this]
   (let [ret
         {:object-name (aget c/rel-map (c/get-id this))}
         relcos (vec (filter (fn [x] (c/is-offline-enabled x)) (get-rel-containers this)))]
     (if (empty? relcos)
       ret
       (assoc ret :children (vec (map get-offline-objects-tree relcos))))))
  (get-offline-changes
   [this]
   (->
    (off/get-change-tree (get-offline-objects-tree this))
    (p/then
     (fn [changes]
       (let [ch-tree
             (prewalk
              (fn [n]
                (if-not (map? n)
                  n
                  (if-let [on (:object-name n)]
                    (assoc n :relationship
                           (aget (@c/container-registry
                                  (aget c/rel-map-reverse on))
                                 "rel"))
                    n))) changes)]
         (when-not (c/empty-data-tree? ch-tree)
           (-> ch-tree clj->js u/create-json)))))))
  (post-offline-changes
   [this cb errb]

   (->
    (get-offline-changes this)
    (p/then (fn [changes]
              (when-not (empty? changes)
                (kk! this "postOfflineChanges" c/post-offline-changes changes  cb errb))))
    (p/then (fn [res]
              (offline-post-finished this (first res))))))
  (save-offline-changes 
   [this cb errb]
   (kk! this "saveOfflineChanges" c/save-offline-changes  cb errb)
   )
  (rollback-offline-changes 
   [this cb errb]
   (kk! this "rollbackOfflineChanges" c/rollback-offline-changes  cb errb)
   )
  (offline-post-finished 
   [this res]
   (u/debug "OFFLINE POST FINISHED")
   (println res)
   ;;(println (u/transit-read res))
   )
  (move-to-uniqueid 
   [this uniqueid cb errb]
   (kk! this "moveto" c/move-to-uniqueid uniqueid cb errb))
  (get-local-data-by-uniqueid
   [this uniqueid]
   (c/get-local-data-by-uniqueid (c/get-id this) uniqueid)
   )
  (register-columns [this columns cb errb]
                    (c/register-columns this  (seq columns) cb errb))
  (deregister-columns [this columns]
                      (c/deregister-columns this (seq columns)))
  (after-fetch 
   [this cb]
   (when-not (aget this "fetch-deferred")
     (aset this "fetch-deferred" (p/get-deferred)))
   (let [prms (aget this "fetch-deferred")]
     (p/then prms cb)));returns a promise
  (get-key-attributes 
   [this cb errb]
   (kk! this "getKeyAttributes" c/get-key-attributes  cb errb))
  (get-field-local-value
   [this column] 
   (c/get-local-data (c/get-id this) (get-currow this) column)
   )
  (get-row-count 
   [this callback errback]
   (kk! this "count" c/mboset-count-with-offline callback errback)
   
   )
  (set-order-by
   [this column  callback errback]
   (kk! this "setOrderBy" c/set-order-by  column callback errback)
   )
  (set-qbe
   [this column value callback errback]
   (kk! this "setQbe" c/set-qbe-with-offline  column value callback errback))
  (set-value
   [this column value callback errback]
   (kk! this "setValue" c/set-value-with-offline  column value callback errback))
  
  (del-row
   [this callback errback]
   (kk! this "delete" c/delete-with-offline callback errback))
  (undel-row
   [this callback errback]
   (kk! this "undelete" c/undelete-with-offline callback errback))
  (fetch-current
   [this cb errb]
   (kk! this "fetch" c/fetch-current cb errb)
   )
  (fetch-data
   [this start numrows cb errb]
   ;;fetching queue is the performance optimization
   ;;many times we can't control how the data in controls is initialized, and the controls just repeadetly call the fetch-data , thus hurting the performance. there will be the queue for fetch, and once is done, the fetch for the same number of rows can be done, otherwise is discarded. This is safe to do because for one container server operations are serialized, and it is not possible to move to other record before the fetch has been finished.
   (assert (and start numrows) "Fetching must have the starting row and the number of rows specified")
   (when-not (c/get-state this :fetch-queue)
     (c/set-states this
                   {:fetch-queue (atom [])})
     ;;the idea is to put the fetch to be performed in the queue, if there is already the same element in the queue, skip the fetch. The underlying promise framework will control the order of execution, we don't need channels,
     )
   (let [fq (c/get-state this :fetch-queue)
         new-fetch {:f-start start :f-numrows numrows}]
     (when-not (some (fn [{:keys [f-start f-numrows]}]
                       (and (= f-start start) (= f-numrows numrows)))
                     @fq)
       (swap!  fq conj new-fetch)
       (kk! (c/get-container this) "fetch" c/fetch-with-local start numrows
            (fn [ok]
              (swap! fq (fn[s] (remove #(= % new-fetch) s)))
              (when cb (cb ok)))
            (fn [err]
              (swap! fq (fn[s] (remove #(= % new-fetch) s)))
              (when errb (errb err)))))))
  (init-data-with-off
   [this start numrows cb errb]
   (fetch-data this start numrows cb errb))
  ;;the callback will not be called if there is skip, but we have to remove the wait cursor
  (reset [this cb errb]
         (kk! this "reset" c/reset-with-offline  cb errb))
  (fetch [this cb errb]
         (kk! this "fetch" fetch-reset-rels  cb errb))
  (save [this cb errb]
        (kk! this "save" c/save cb errb))
  (add-new-row [this callback errback]
               (kk! this "addNewRow" c/add-at-end  callback errback)
               )
  (add-new-row-at [this rownum callback errback]
                  (let [_id (c/get-id this)
                        nrs  (c/get-local-data-fetch-size _id)]
                    (c/trim-object-data _id rownum);clear data from local before the fetch
                    (kk! this "addNewRow" c/add-at-index-with-offline rownum callback errback)
                    (fetch-data this   rownum (inc (- nrs rownum)) callback errback)
                    (move-to-row this rownum callback errback)
                    ;;no need to wait on promises, both fetch-data and move-to-row use internally command pipeline, and will be executed just when the previous one has finished with callback
                    ;;                    (->
                    ;;                     (kk! this "addNewRow" c/add-at-index-with-offline rownum callback errback)
                    ;;                     (p/then (fn [ok] (fetch-data this   rownum (inc (- nrs rownum)) callback errback)))
                    ;;                     (p/then (fn [ok] (move-to-row this rownum callback errback))))
                    ))
  (move-to-row
   [this rownum cb errb]
   (kk! this "move"
        c/move-to-with-offline  rownum cb errb))
  (get-currow [this]
              (c/get-state this :currrow))
  (get-local-data [this rownum]
                  (let [_rn (if rownum (js/parseInt rownum) (get-currow this))]
                    (c/get-local-data-all-attrs (c/get-id this) _rn)))
  (get-qbe [this cb errb]
           (kk! this "getQbe"
                c/get-qbe-with-offline
                (fn [e]
                  (println "got qbe " e)
                  (let [ qbe (-> (get e 0)  u/vec-to-map)]
                    (when qbe
                      (aset this "qbe" qbe)
                      (cb qbe))))
                errb
                ))
  App
  (mbo-command
   [this command argControl callback errback]
   (this-as
       this
     (kk! this command c/run-mbo-command  command argControl callback errback)))
  (mboset-command
   [this command argControl callback errback]
   (this-as this
     (kk! this command c/run-mboset-command  command argControl callback  errback)))
  Receivable
  (get-receive-functions
   [this]
   {"fetch-finished" (fn [_]
                       (when-let [dfrd (aget this "fetch-deferred")]
                         (p/callback dfrd)
                         (js-delete this "fetch-deferred")))
    "set-control-index" (fn [ev]
                          (let [currow (get ev :currrow)
                                prev-row (get ev :prevrow)]
                            (when-not (or
                                       (= -1 (js/parseInt currow))
                                       (= currow prev-row))
                              (doseq [_cnt  (get-rel-containers this) ]
                                (re-register-and-reset _cnt nil nil)))))
    "reset" (fn [_]
              (doseq [_cnt (get-rel-containers this)]
                (re-register-and-reset _cnt nil nil))
              (on-reset this))
    }
   ))

(def-comp AppContainer [mboname appname] MboContainer
  (^override fn* []
   (this-as
       this
     (googbase this mboname)
     (let [deferred (promise-chan)]
       (c/toggle-state this :deferred deferred)
       (kk! this "init" c/set-current-app-with-offline  (.toUpperCase appname)
            (fn [ok] (go (put! deferred ok)))
            nil)))) 
  Container
  (late-register
   [this]
   (kk-nocb! this "currapp" c/set-current-app-with-offline  (.toUpperCase appname)))
  (use-stored-query
   [this queryName]
   (c/use-stored-query (c/get-id this) queryName))
  (^override init-data-with-off ;when the login happens after the controls have been registerd from offline, the changes get wiped out by the reset. This will post the data change after the data is set
   [this start numrows cb errb]
   (when
       (and
        (c/is-offline-enabled this)
        (not (aget this "offlinePosting"))
        (not @c/is-offline);don't try to post offline when offline
        )
     (aset this "offlinePosting" true)
     (->
      (off/exist-table? (aget c/rel-map (c/get-id this)) :raw)
      (p/then
       (fn [ex?]
         (when ex?
           (let [table-name (aget c/rel-map (c/get-id this))]
             (->
              (get-offline-changes this)
              (p/then (fn [changes]
                        (when-not (empty? changes)
                          (kk! this "postOfflineChanges" c/post-offline-changes changes  cb errb))))
              (p/then (fn [res]
                        (off/debug-table table-name)
                        (offline-post-finished this (first res))
                        (aset this "offlinePosting" false)
                        (off/delete-old-records (aget c/rel-map (c/get-id this))))))))))))
   (fetch-data this start numrows cb errb))
  App
  (access-to-option [this option callback errback]
                    (kk! this "access" c/access-to-option  option callback errback))
  (get-app
   [this]
   appname)
  )

(def-comp RelContainer [mbocont rel] MboContainer
  (^override fn* []
   (this-as this
     (.call BaseComponent this);super-super konstruktor
     (c/add-container-to-registry this)
     (let [deferred (promise-chan)]
       (c/set-states this
                     {:currrow -1
                      :uniqueid (p/get-deferred)
                      :offlineenabled false
                      :iscontainer true
                      :rel-containers []
                      :deferred deferred
                      :parentid (c/get-id mbocont)})
       (mm/kk-branch! mbocont this "init" c/register-mboset-byrel-with-offline rel (c/get-id mbocont)
                      (fn [ok] (go (put! deferred ok))) nil)
       (aset this "appname" (aget mbocont "appname"))
       (add-child mbocont this)
       (c/toggle-state mbocont :rel-containers (conj (c/get-state mbocont :rel-containers) this)))))
  Offline
  (is-offline-enabled
   [this]
   (c/is-offline-enabled (get-parent this)));;it doesn't make sense to have offline enabled for rel container, but not for main. Basically the change will allow to define the offline enabled only once in the project(for the app container), all the rules should be inherited from that
  Container
  (^override late-register
   [this]
   (mm/kk-branch-nocb! mbocont this "register" 
                       c/register-mboset-byrel-with-offline rel (c/get-id mbocont)))
  (^override re-register-and-reset [this cb errb]
   ;;   (u/debug "calling re-registration of  relcontainer " (c/get-id this))

   (let [id (c/get-id this)
         dfrd (promise-chan)];so the reference to it is kept in the closure. If after the first call this is cancelled, the first call will not proceed.
     (c/toggle-state this :deferred dfrd)
     (p-deferred-on dfrd
                    (doseq [c  (get-children this)]
                      (when-not (c/get-state c :iscontainer)
                        (clear-control c)
                        (init-data c)))
                    (when cb (cb this)))
     (c/re-register-mboset-byrel-with-offline
      id rel (c/get-id mbocont)
      (fn [ok]
        (go (put! dfrd ok)))
      (fn [err]
        (go (put! dfrd err))))))
  Foundation
  (dispose 
   [this]
   (c/toggle-state mbocont :rel-containers (filter (fn [c] (not= c this)) (c/get-state mbocont :rel-containers))))
  Receivable
  (^override send-command
   [this command command-f command-cb command-errb]
   (let [cch (aget this "command-channel")
         dd1 (c/get-state mbocont :deferred)
         dd2 (c/get-state this :deferred)
         dd (if (or (= command "init") (not dd2))
              dd1
              dd2
              )]
     (p-deferred-on dd
                    (go (put! cch [command command-f command-cb command-errb]))))))

;;uniqueid will be optional. That will be used instead of uniquembocontainer when we want to establish the hierarchy like the parent container (with setOwner in mbo), right now just for GraphQL. The point is that we need to preserve the access paths same as for the parent, so it will not give access denied exception
(def-comp SingleMboContainer [mbocont contuniqueid] RelContainer
  (^override fn* []
   (this-as this
     (.call BaseComponent this);super-super konstruktor
     (c/add-container-to-registry this)
     (let [deferred (promise-chan)]
       (c/set-states this
                     {:currrow -1
                      :uniqueid (p/get-deferred)
                      :offlineenabled false
                      :iscontainer true
                      :rel-containers []
                      :deferred deferred
                      :parentid (c/get-id mbocont)})
       (kk-branch! mbocont this "init" c/register-mboset-with-one-mbo-with-offline (c/get-id mbocont) contuniqueid
                   (fn [ok]
                     (go (put! deferred ok))) nil))
     (aset this "appname" (aget mbocont "appname"))
     (add-child mbocont this)
     (c/toggle-state mbocont :rel-containers (conj (c/get-state mbocont :rel-containers) this))))
  Container
  (^override re-register-and-reset
   [this cb errb]
   (let [idcont (c/get-id mbocont)
         _unid (aget this "contuniqueid")
         dfrd (promise-chan)]
     (c/toggle-state this :deferred dfrd)
     (p-deferred-on dfrd
                    (doseq [c  (get-children this)]
                      (clear-control c)
                      (init-data c)))
     (c/re-register-mboset-with-one-mbo-with-offline
      (c/get-id this)
      idcont
      _unid
      (fn [ok]
        (go (put! dfrd ok)))
      (fn [err]
        (go (put! dfrd err)))))))

(def-comp UniqueMboAppContainer [mboname appname uniqueId] MboContainer
  (^override fn* []
   (this-as
       this
     (let [deferred (promise-chan)]
       (googbase this mboname)
       ;;                 (c/toggle-state this :deferred (kk-nocb! this "setUniqueApp" c/set-unique-app (.toUpperCase appname) (.toString uniqueId) ))
       (c/toggle-state this :deferred deferred)
       (kk! this "init" c/set-unique-app (.toUpperCase appname) (.toString uniqueId)
            (fn [ok] (go (put! deferred ok))) nil))))
  Container
  (get-unique-id [this]
                 uniqueId)
  Receivable
  (^override get-receive-functions
   [this]
   {"fetch-finished" (fn [_]
                       (when-let [dfrd (aget this "fetch-deferred")]
                         (p/callback dfrd)
                         (js-delete this "fetch-deferred")))
    "reset" (fn [_]
              (doseq [_cnt (get-rel-containers this)]
                (reset _cnt nil nil))
              (on-reset this))}))

(def-comp UniqueMboContainer [mboname uniqueid] MboContainer
  (^override fn* []
   (this-as
       this
     (googbase this mboname)
     (let [deferred (promise-chan)]
       ;;                 (c/toggle-state this :deferred (kk-nocb! this "setUniqueId" c/set-unique-id  (.toString uniqueid) ))
       (c/toggle-state this :deferred deferred)
       (kk! this "init" c/set-unique-id  (.toString uniqueid)
            (fn [ok]
              (go (put! deferred ok))) nil))))
  Container
  (get-unique-id [this]
                 uniqueid)
  Receivable
  (^override get-receive-functions
   [this]
   {"fetch-finished" (fn [_]
                       (when-let [dfrd (aget this "fetch-deferred")]
                         (p/callback dfrd)
                         (js-delete this "fetch-deferred")))
    "reset" (fn [_]
              (doseq [_cnt (get-rel-containers this)]
                (reset _cnt nil nil))
              (on-reset this))}))


(mm/def-comp QueryMboContainer [appContainer] MboContainer
  (^override fn* []
   (this-as
       this
     (.call BaseComponent this);super-super konstruktor
     (c/add-container-to-registry this)
     (let [deferred (promise-chan)]
       (c/set-states this
                     {:currrow -1
                      :uniqueid (p/get-deferred)
                      :offlineenabled false
                      :iscontainer true
                      :rel-containers []
                                        ;                                       :deferred (kk-branch-nocb! appContainer this "init" c/register-query-mboset  (.toUpperCase (aget appContainer "appname")))
                      :deferred deferred
                      })
       (kk-branch! appContainer this "init" c/register-query-mboset  (.toUpperCase (aget appContainer "appname"))
                   (fn [ok] (go (put! deferred ok))) nil)))))

(mm/def-comp BookmarkMboContainer [appContainer] MboContainer
  (^override fn* []
   (this-as
       this
     (.call BaseComponent this);super-super konstruktor
     (c/add-container-to-registry this)
     (let [deferred (promise-chan)]
       (c/set-states this
                     {:currrow -1
                      :uniqueid (p/get-deferred)
                      :offlineenabled false
                      :iscontainer true
                      :rel-containers []
                      ;;                                       :deferred  (kk-branch-nocb! appContainer this "init" c/register-bookmark-mboset  (.toUpperCase (aget appContainer "appname")))
                      :deferred deferred
                      })
       (kk-branch! appContainer this "init" c/register-bookmark-mboset  (.toUpperCase (aget appContainer "appname"))
                   (fn [ok] (go (put! deferred ok))) nil)))))

(mm/def-comp InboxMboContainer [] MboContainer
  (^override fn* []
   (this-as
       this
     (.call BaseComponent this);super-super konstruktor
     (c/add-container-to-registry this)
     (let [deferred (promise-chan)]
       (c/set-states this
                     {:currrow -1
                      :uniqueid (p/get-deferred)
                      :offlineenabled false
                      :iscontainer true
                      :rel-containers []
                      ;;                                       :deferred  (kk-nocb!  this "init" c/register-inbox)
                      :deferred deferred
                      })
       (kk!  this "init" c/register-inbox
             (fn [ok] (go (put! deferred ok))) nil)))))

(mm/def-comp PersonMboContainer [] MboContainer;will have just one record with the logged in person
  (^override fn* []
   (this-as
       this
     (.call BaseComponent this);super-super konstruktor
     (c/add-container-to-registry this)
     (let [deferred (promise-chan)]
       (c/set-states this
                     {:currrow -1
                      :uniqueid (p/get-deferred)
                      :offlineenabled false
                      :iscontainer true
                      :rel-containers []
                      :deferred deferred
                      })
       (kk!  this "init" c/register-person-mboset
             (fn [ok] (go (put! deferred ok))) nil)))))

(def-comp ComponentAdapter [container columns norows] BaseComponent
  (fn* []
       (this-as this
         (let [cb-handler (get-callback-handler this)
               err-handler (get-errback-handler this)
               vcols (seq columns)]
           (googbase this)
           (add-child container this)
           (let [deferred (promise-chan)]
             (c/set-states this {:iscontainer false
                                 :deferred deferred
                                 :columns vcols
                                 :receiver true
                                 })
             (c/register-columns container vcols
                                 (fn [ok] (cb-handler ok) (go (put! deferred ok)))
                                 (fn [err] (err-handler err)))))))
  ControlData
  (init-data
   [this]
   (initControlDataRows this (if norows norows 1))
   )
  UI
  (set-enabled [this enable])
  (clear-control [this])
  (on-set-max-value [this column value])
  (on-set-readonly [this flag])
  Component
  (get-container [this] container)
  Foundation
  (dispose [this]
           (c/deregister-columns  (c/get-id container)  columns))
  (add-virtual-column [this ind column metadata]
                      (p-deferred this (add-control-column this ind column metadata)))
  (set-max-value [this column value]
                 (mm/c! this "set-value" set-value  container column value)
                 )
  MessageProcess
  (on-set-control-index [this row])
  (on-fetched-row [this row])
  (on-reset [this]
            (clear-control this)
            (set-enabled this false))
  (on-set-fieldflag [this row field _flags])
  (on-fetch-finished [this])
  Receivable
  (get-receive-functions
   [this]
   {"addmbo" #() ; #(u/debug "called  addmbo in component adapteru for " %)
    "fetched-row" #(on-fetched-row this %)
    "reset" (fn [_] (on-reset this))
    "set-control-index" (fn[e]
                          (on-set-control-index this (get e :currrow)))
    "update-mboflag" (fn [e]
                       (let [row (get e :rownum)]
                         (when (= row (get-currow (c/get-container this)))
                           (on-set-readonly this (c/str->bool (get e :readonly))))))
    "update-fieldflag" (fn [e] (on-set-fieldflag this (get e :rownum) (get e :field) (get e :flag)))
    "update-control-data"
    (fn[e]
      (let [rownum (get e :rownum)
            column (get e :column)
            value (get e :value)
            ]
        (when
            (= rownum (get-currow (c/get-container this)))
          (on-set-max-value this column value)))
      )
    })
  )

(def-comp VisualComponent [] BaseComponent
  (fn* []
       (this-as this
         (googbase this)
         (c/toggle-state this :displayed (promise-chan))))
  UI
  (render-deferred
   [this]
   (p-deferred this
               (render this)
               (on-render-internal this);not to be extended by the user
               (on-render this)
                                        ;               (when-let [dispd (c/get-state this :displayed)]
                                        ;                 (p/callback dispd "done"))
                                        ;don't automatically call displayed, this will be a separate function for setting it, so controls can notify when it is actually displayed. In react it will be done from the separate function, for the plain web rendering it will be done in render
               ))
  (mark-as-displayed
   [this]
   (go (put! (c/get-state this :displayed) :displayed)))
  (render
   [this]
   (throw (js/Error. "render not implemented")))
  (on-render-internal [this])
  (on-render
   [this])
  (add-ui-listeners
   [this kv]
   (doseq [[k v] kv]
     (add-ui-listener this k v )
     )
   );key value map, used just internally
  (add-ui-listener
   [this key f]
   (throw (js/Error. "addUiListener not implemented")));concrete implemetation
  (get-ui-event-value
   [this key ev-val]
   (throw (js/Error. "getUiEventValue not implemented")))
  Component
  (get-col-attrs [this]
                 (c/get-state this :colAttrs))
  )

(mm/def-comp ListContainer [mbocont column qbe] MboContainer
  (^override fn* []
   (this-as
       this
     (.call BaseComponent this);super-super konstruktor
     (c/add-container-to-registry this)
     (let [deferred (promise-chan)]
       (c/set-states this
                     {:currrow -1
                      :uniqueid (p/get-deferred)
                      :offlineenabled false
                      :iscontainer true
                      :rel-containers []
                      :deferred deferred
                      }
                     )
       (kk-branch! mbocont this "init" c/register-list-with-offline (c/get-id mbocont) column qbe
                   (fn [ok] (go (put! deferred ok))) nil))
     (add-child mbocont this)))
  Offline
  (is-offline-enabled
   [this]
   (c/is-offline-enabled mbocont)))

(defprotocol CommandProcess
  (pre-process-command-callback [control e])
  )

(mm/def-comp CommandContainer [mbocont command args] MboContainer
  (^override   fn* []
   (this-as
       this
     (.call BaseComponent this);super-super konstruktor
     (c/add-container-to-registry this)
     (c/add-peer-control nil (c/get-id this))
     (let [fap (apply partial  command (.getId this) (js->clj args))
           deferred (promise-chan)]
       (c/set-states this
                     {:currrow -1
                      :uniqueid (p/get-deferred)
                      :offlineenabled false
                      :iscontainer true
                      :rel-containers []
                      :deferred  deferred}
                     )
       (mm/kk-branch-noargs! mbocont this "init" fap
                             (fn [e]
                               (pre-process-command-callback this e)
                               (go (put! deferred e)))
                             (fn [err] ((get-errback-handler this) err) )))
     (add-child mbocont this)))
  CommandProcess
  (pre-process-command-callback [this e]
                                (c/process-register-list-callback-event (c/get-id this) e)
                                ((get-callback-handler this) e)))

(defn- smart-fill-with-lookup
  [container column value]
  (let [metadata (aget column "metadata")
        attribute (:attributeName metadata)
        sm-cont (CommandContainer. container c/smart-fill [(c/get-id container) attribute value])]
    (mm/p-deferred sm-cont
                   (c/mboset-count-with-offline
                    (.getId sm-cont)
                    (fn[e]
                                        ;         (u/debug "Dobio sam count:" e)
                      (if (= 1 (first e))
                        (dispose sm-cont)
                        (show-smartfill-list  column sm-cont)))))))

;;Reserved metadata for the field:
;;hasLookup - if true it will call the addLookup
;;listColumns -  if hasLookup this is the list of columns for the lookup
(def-comp TextField [metadata] VisualComponent
  (fn* []
       (this-as this
         (googbase this)
         (c/set-states
          this {:receiver false
                :deferred (promise-chan)
                })))
  App
  (callback-handler
   [this command evTarget]
                                        ;don't know why in original 1.0 code this is here, if I don't find out I will delete it
   )
  (^override errback-handler  [this error-text error-code error-group error-key]
                                        ;(u/debug "pozvan je errback handler od textfielda za komandu " command " i error " error)
   (replace-from-local this)
   (set-field-focus (get-parent this) this)
   ((get-errback-handler nil) [[error-code error-text error-group error-key] nil nil])) ;;force global error handler
  UI
  (on-render
   [this]
                                        ;   (add-field-ui-listeners (get-parent this) this {:change (fn [ev]
                                        ;                                              (change-maximo-value this
                                        ;                                                                   (get-ui-event-value this :change ev)))})
   (when (:hasLookup metadata) (add-lookup-internal this))
   (when (:canSmartFill metadata) (aset this "canSmartFill" true))
   (mm/loop-arr [_acc (:custom-actions metadata)]
                (let [listenF (aget _acc 0)
                      actF (aget _acc 1)
                      deferred (aget _acc 2)]
                  (add-action this listenF actF deferred)))
   (when-let [custom-transform (c/get-column-attribute this (get-column this) "custom-transform")]
     (let [tr-func (aget custom-transform 0)]
       (tr-func this)))
   )
  (set-enabled
   [this enable]
   (mm/p-deferred-on (c/get-state this :displayed)
                     (let [parent (get-parent this)]
                       (if (or (:readOnly metadata) (and parent (not (.isEnabled parent))))
                         (set-readonly this true)
                         (set-readonly this false)))))
  (set-readonly 
   [this flag]
   (throw (js/Error. "setReadOnly not implemented")))
  (set-required [this flag]
                (throw (js/Error. "setRequired not implemented"))
                )
  Field
  (get-metadata
   [this]
   (u/to-js-obj
    (aget this "metadata")))
  (changed-row [this row])
  (local-value
   [this]
   (get-field-local-value (get-parent this) (get-column this))
   )
  (add-action
   [this listenF actF deferred]
   (listenF this
            (fn [e]
              (let [parent (get-parent this)
                    grid (get-parent parent)]
                (if (and  grid deferred)
                  (when-let [vrs (c/get-state grid :virtual-deferreds)]
                                        ;ovo treba da bude na gridu iz prostog razloga sto je red na kome okidam drugaciji od reda na kome se desava move-to
                    (let [deferred (p/get-deferred)]
                      (.push vrs deferred)
                      (p-deferred-on deferred (actF this))))
                  (actF this))))))
  (remove-action
   [this removalF]
   (removalF this); removal action ce biti bilo kakav unlisten, tako da mora da se prosledjuje ovako, sve ostalo je previse komplikovano
   )
  (add-lookup-internal
   [this]
   (let [metadata (aget this "metadata")
         maxtype (:maxType metadata)
         ]
     (when (and (not= false (:canSmartFill metadata))
                (or (= maxtype "ALN") (= maxtype "UPPER") (= maxtype "LOWER")))
       (aset this "canSmartFill" true))
     (add-lookup this)))
  (add-lookup 
   [this]
                                        ;should display or not display lookup icon (don't know how to do that for native), and listen on event to show the lookup
   (throw (js/Error. "addLookup not implemented"))
   )
  (show-lookup
   [this]
   (let [metadata (aget this "metadata")
         attributeName (:attributeName metadata)
         maxType (:maxType metadata)
         hasLD (:hasLD metadata)
         isALNDomain (:isALNDomain metadata)]
     (cond  
       hasLD (show-ld-lookup this)
       (= "DATE" maxType) (show-date-lookup this)
       (= "DATETIME" maxType) (show-date-time-lookup this)
       :default (if-let [columns (get-list-columns this)]
                  (show-list this columns)
                  (u/debug "No lookup for the column")))))
  (show-smartfill-list [this smartfillContainer]
                       (when-let [ columns  (get-list-columns this)]
                         (get-list-dialog this smartfillContainer columns)))
  (show-gl-lookup
   [this orgid]
   (not-implemented))
  (get-list-columns
   [this]
   (let [metadata (aget this "metadata")
         isALNDomain? (:isALNDomain metadata)
         listColumns (:listColumns metadata)]
     (if listColumns 
       listColumns
       (when isALNDomain?
         #js ["value" "description"]))))
  (set-ld-value
   [this value]
   (kc! this "setLdValue" c/set-value-with-offline
        (str (get-column this) "_LONGDESCRIPTION")
        value))
  (get-list-labels
   [this])
  (show-ld-lookup [this]
                  (throw (js/Error. "showLdLookup not implemented"))
                  )
  (display-label [this label]
                 (throw (js/Error. "displayLabel not implemented")))
  (set-label [this label]
             (display-label this label)
             (aset this "metadata"
                   (assoc (aget this "metadata")  "title" label)))
  (show-date-lookup [this]
                    (throw (js/Error. "showDateLookup not implemented"))
                    )
  (show-date-time-lookup
   [this]
   (throw (js/Error. "showDateTimeLookup not implemente"))                )
  (set-flag [this [readonly? required?]]
            (set-required this required?)
            (let [parent (.getParent this)
                  pe (aget parent "enabled")]
              (set-readonly this (if pe readonly? true)))
            )
  (change-maximo-value
   [this value]
   ;;if value is of type js/date, alwayws format it to Maximo date type
   (let [ctrl (get-parent this)
         container (aget ctrl "container")
         cid (c/get-id container)
         column (:attributeName metadata )
         curr-row (get-currow container)
         smartfill? (aget this "canSmartFill")
         _value (if (instance? js/Date value)
                  (c/formatToMaximoDate value)
                  value)
         ]
                                        ;TODO smartfill will not work in the offline mode. It shouldn't be hard to implement it, but definitely this is low priority
     (if (and smartfill? (not= "" value) (not @c/is-offline)) 
       (smart-fill-with-lookup container this _value)
       (kc! this "set-value" c/set-value-with-offline column _value))))
  (get-list-dialog [this listContainer columns]
                   (get-field-list-dialog (get-parent this) this listContainer columns)
                   )
  (set-field-value [this value]
                   (throw (js/Error. "setFieldValue not implemented")))              
  (get-column [_]
              (:attributeName metadata))
  (show-list [this columns]
             (let [ctrl (get-parent this)
                   container (aget ctrl "container")
                   diag  (get-list-dialog this (ListContainer. container (get-column this) false) columns)
                   ]
               (when diag
                 (aset diag "parent-field" this))))
  (replace-from-local [this]
                      (let [ctrl (get-parent this)
                            container (aget ctrl "container")
                            cid (c/get-id container)
                            column (:attributeName metadata)
                            curr-row (get-currow container)
                            rowid (c/get-id-from-row cid curr-row)
                            ]
                        (set-row-field-value (get-parent this) this
                                             (c/get-local-data cid curr-row column))))
  (set-focus [this]
             (throw (js/Error. "setFocus not implemented")))
  Component
  (get-col-attrs [this]
                 (c/get-col-attrs (get-parent this))
                 )
  (get-container [this]
                 (let [ctrl (get-parent this)]
                   (c/get-container ctrl)))

  )

(def-comp CheckBox [metadata] TextField
                                        ;since this is the abstract checkbox, used both for the native and the HTML5 components, there will be nothing particullarly checkboxy here - the only thing is that the change will be done after the move to row is finished- becuase change event will be triggered when the actual moving to another row starts, we have to wait until is done before the value is set.
  (^override fn* []
   (this-as this
     (googbase this metadata)))
  UI
  (^override on-render
   [this]
   (let [parent (get-parent this)
         pparent (get-parent parent)]
     (if pparent ; that is if this is the grid
       (add-ui-listeners this {:change (fn [ev]
                                         (after-move pparent
                                                     (change-maximo-value
                                                      this
                                                      (get-ui-event-value this :change ev))))})
       (add-ui-listeners this {:change (fn [ev]
                                         (change-maximo-value
                                          this
                                          (get-ui-event-value this :change ev)))})
       ))))

(def-comp VirtualActionField [metadata] TextField
  (^override fn* []
   (this-as this
     (googbase this metadata)))
  UI
  (^override on-render
   [this]
   (let [parent (get-parent this)
         pparent (get-parent parent)]
     (listen-action this
                    (fn [ev]
                      (let [action (:action metadata)]
                        (if pparent
                          (when-let [vrs (c/get-state pparent :virtual-deferreds)]
                            
                            (let [deferred (p/get-deferred)]
                              (.push vrs deferred)
                              (mm/p-deferred-on deferred (action this))))
                          (action this)))))
     (mm/loop-arr [_acc (:custom-actions metadata)]
                  (let [listenF (aget _acc 0)
                        actF (aget _acc 1)
                        deferred (aget _acc 2)]
                    (add-action this listenF actF deferred)))
     (when-let [custom-transform (c/get-column-attribute this (get-column this) "custom-transform")]
       (let [tr-func (aget custom-transform 0)]
         (tr-func this))))))

(def-comp Section [container columns] VisualComponent
  (fn* []
       (this-as
           this
         (googbase this)
         (let [cb-handler (get-callback-handler this)
               err-handler (get-errback-handler this)
               vcols (seq columns)
               deferred (promise-chan)]
           (c/set-states this
                         {:isconatiner false
                          :receiver true
                          :columns columns
                          :deferred deferred})
           (c/register-columns container  vcols
                               (fn [ok]
                                 (cb-handler)
                                 (go (put! deferred ok)))
                               err-handler)
           (add-child container this))))
  Component
  (get-container
   [this]
   container)
  Container
  (get-field-local-value
   [this column]
   (get-field-local-value container column))
  App
  (callback-handler [this command  ev-target]
                    )
  (prepare-call [this command ev-target]
                )
  UI
  (is-enabled [this] (c/get-state this :enabled))
  (set-enabled [this enable]
               (c/toggle-state this :enabled enable)
               (aset this "enabled" enable)
                                        ;               (mm/loop-arr [c  (get-children this)] 
                                        ;                            (set-enabled c enable)
                                        ;                            (set-readonly c (not enable)))
               )
  (draw-section [this] (throw (js/Error. "drawSection not defined")))
  (get-insertion-point [this] (throw (js/Error. "getInsertion point not defined")))
  (add-rendered-child [this rendered-child child] (throw (js/Error. "addRenderedChild not implemented")))
  (^override render
   [this]
   (let [vs (draw-section this)
         _cols (get-columns this)]
     (c/toggle-state
      this
      :column-map
      (loop [columns _cols colmap {}];when writing the value, lookup field by column name, performace optimization
        (if (empty? columns)
          colmap
          (let [col (first columns)
                col-metadata  (c/get-attribute-metadata-with-col-attrs
                               this
                               col)
                field (create-field this col-metadata)
                _ (add-child this field)
                rendered-field (render-deferred field)]
            
            (add-rendered-child this rendered-field field)
            (add-field-ui-listeners
             this field {:change (fn [value]
                                   (change-maximo-value field value))})
            (recur (rest columns) (assoc colmap (.toUpperCase col) field))))))
     (init-data this)
     vs))
  (on-set-readonly
   [this flag]
                                        ;                   (u/debug "seting the row readonly:" mxrow flag)
   (set-enabled this (not flag))
   (when-not flag
     (set-flags-from-local this)))
  (clear-control
   [this]
   (doseq [fld  (get-children this)]
     (set-row-field-value this fld "")))
  (on-set-max-value
   [this column value]
   (doseq [fld (get-children this)]
     (when (= (get-column fld) (.toUpperCase column))
       (set-row-field-value this fld value))))
  Foundation
  (dispose
   [this]
   (when-not @c/is-offline (c/deregister-columns  (c/get-id container)  columns)))
  (add-virtual-column
   [this pos column metadata]
   (let [existing-cols (vec (c/get-state this :columns))
         _pos (if pos pos  (count existing-cols))
         [befcols aftcols] (split-at _pos existing-cols)
         vcol (:attributeName metadata)
         deferred (promise-chan)] ;;the existence of this means the virtual column is bound to the real column with this attribute
     (c/toggle-state this :columns  (concat befcols [column] aftcols))
     (c/add-col-attrs this column metadata)
     (when vcol
       (c/set-states this
                     {:deferred deferred})
       (c/register-columns (c/get-container this) [vcol] (fn [ok] (go (put! deferred ok))) nil)
       )))
  Row
  ;;one level of indirection required for React and similar frameworkks(where state is kept in the top level component)
  (get-columns
   [this]
   (c/get-state this :columns))
  (set-row-values
   [this colvals]
   
   (doseq [k (keys colvals)]
     (set-row-value this k (get colvals k))))
  (set-row-value
   [this col value]
   (when-let [field (get-field this col)]
     (set-field-value field value)))
  (set-row-field-value
   [this field value]
   (set-field-value field value))
  (set-row-flags
   [this colflags]
   (doseq [k (keys colflags)]
     (set-field-flag this k (get colflags k)))
   )
  (set-field-flag
   [this field flag]
   (set-flag field flag))
  (set-field-label
   [this field label]
   (set-label field label))
  (row-changed
   [this field]
   (changed-row field this))
  (set-field-enabled
   [this field enabled?]
   (set-enabled field enabled?))
  (set-field-required
   [this field required?]
   (set-required field required?))
  (skip-select-action [this skip]) ; checkbox calls this, so jsut plug it in artificially (it is used for grid rows)
  (get-field
   [this column]
   (when-not (= (type column) js/String)
     (println "!!!!!!!!!!!!!!!!!!!!!!MOSA")
     (println column))
   (let [column-map (c/get-state this :column-map)]
     (when column-map (column-map (.toUpperCase column)))))
  (create-field
   [this column-metadata]
   (let [col-metadata  column-metadata
         tp (get col-metadata "maxType")
         fld (condp = tp
               "VIRTUAL" (VirtualActionField. col-metadata)
               "YORN" (CheckBox. col-metadata)
               (TextField. col-metadata))]
     fld
     ))
  (get-fields
   [this columnz]
   (loop [ch (get-children this) rez []]
     (if (empty? ch)
       rez
       (let [c (first ch)
             _rez (if (some #(= (.toUpperCase %) (get-column c)) columnz)
                    (conj rez c)
                    rez)]
         (recur (rest ch) _rez)))))
  (set-field-focus
   [this field]
   (set-focus field))
  (add-default-lookups
   [this columnz]
   (mm/p-deferred-on (c/get-state this :displayed)
                     (mm/loop-arr [f (get-fields this columnz)]
                                  (add-lookup f))))
  (set-col-label
   [this column label]
   (mm/p-deferred-on-attr this "displayed"
                          (mm/loop-arr [f (get-fields this [column])]
                                       (set-field-label this f label))))
  (add-field-transform 
   [this column transformF]
   (c/add-col-attr this column "custom-transform" transformF)
                                        ;now just apply the transform if the field is in the document
   (let [f (get-field this column)]
     (p-deferred-on (c/get-state f :displayed)
                    (when-let [ff (aget transformF 0)]
                      (ff f)))))
  (remove-field-transform 
   [this column]
   (let [f (get-field this column)
         transformF (c/get-column-attribute this column "custom-transform")]
     (p-deferred-on (c/get-state f :displayed)
                    (when-let [ff (aget transformF 1)];removal function
                      (ff f))))
   (c/remove-col-attr this column "custom-transform"))
  (add-field-action
   [this column listenF actionF ]
   (let [deferred (aget this "deferred")]
     (mm/p-deferred-on (c/get-state this :deferred)
                       (let [custom-actions
                             (if-let [cas (c/get-column-attribute this column "custom-actions")]
                               cas
                               (js/Array.)
                               )]
                         (ar/conj! custom-actions (js/Array. listenF  actionF deferred))
                         (c/add-col-attr this column "custom-actions" custom-actions))
                       (add-action (get-field this column) listenF actionF deferred))))
  (remove-field-action
   [this column actionF removalF]
   (when-let [cas (c/get-column-attribute this column "custom-actions")]
     (let [indF (ar/index-of cas actionF)]
       (when (not= indF -1)
         (ar/remove-at cas indF))))
   (remove-action (get-field this column) removalF))
  (add-field-ui-listeners
   [this field listener-map]
   (add-ui-listeners field listener-map))
  Dialog
  (get-field-list-dialog
   [this field list-container dialog-columns]
   (throw (js/Error. "getFieldListDialog not implemented")))
  ControlData
  (set-flags-from-local
   [this]
   (when-let [flags (c/get-local-flags (c/get-id container) (js/parseInt (get-currow container)))]
     (doseq [field (keys flags)]
       (when-let [_f (get-field this field)]
         (set-field-flag this _f (flags field))))))
  (add-row 
   [this x]
   (let [_xrow (:row x)]
     (when ( = _xrow (get-currow container))
       (set-enabled this true)
       (let [data (:data x)
             ks (keys data)
             flags (:flags x)]
         (doseq [k ks]
           (if (= "readonly" k)
             (on-set-readonly this (get data k))
             (do
               (on-set-max-value this k (get data k))
                                        ;changed-row triggers redraw of the field lists, it should be done only when the row changes
               (let [srow (aget this "sectionrow")]
                 (when (not= srow _xrow)
                   (aset this "sectionrow" _xrow)
                   (when-let [fld (get-field this k)]
                     (row-changed this  fld)))))))
         (doseq [field (keys flags)]
           (when-let [_f (get-field this field)]
             (set-field-flag this _f (get flags field))))))))
  (init-data-from
   [this  start-row]
   (mm/p-deferred this 
                  (init-data-from-nd this start-row)
                  ))
  (init-data-from-nd
   [this  start-row]
   (when-not (aget container "fetching")
     (mm/c! this "fetch" init-data-with-off container start-row 1)
     ))
  (init-data
   [this]
   (mm/p-deferred
    this
    (let [currow (get-currow container)
          cb-handler (get-callback-handler this)
          err-handler (get-errback-handler this)]
      (if (or (not currow) (= -1 currow))
        (init-data-with-off container 0 1
                            (fn [ok]
                              (when cb-handler (cb-handler ok))
                              (c/move-to-with-offline (c/get-id container) 0))
                            (fn [err]
                              (when err-handler (err-handler err))))
        (init-data-from-nd this currow)))))
  MessageProcess
  (on-set-control-index
   [this row]
   "when the container row has been changed, we have to fetch the data for the section(not all the columns might have been fetched). This should not be done while the fetching from the container is in progress, for the perfomanse reasons"
   (when-not (= -1 (js/parseInt row))
     (if-let [fc (aget container "fetching")]
       (when-not (aget this "fpause")
         (aset this "fpause" true)
                                        ;performance problems, every on-set-indexs triggers fetch, do it just once for the main grid fetch
         
         (p/then fc (fn [fullfill]
                      (aset this "fpause" false)
                      (mm/c! this "fetch" fetch-data container (get-currow container) 1))))
       (mm/c! this "fetch" fetch-data container row 1))))
  (on-fetched-row [this x]
                  (when (:row x)
                    (add-row this x)))
  (on-reset [this]
            (clear-control this)
            (set-enabled this false)
            )
  (on-set-fieldflag
   [this row field _f ]
   (let [readonly? (aget _f 0)
         required? (aget _f 1)]
     (when  (= row (get-currow container))
       (when-let [f  (get-field this field)]
         (set-field-enabled this f (not  (c/str->bool readonly?)))
         (set-field-required this f (c/str->bool required?))))))
  Receivable
  (get-receive-functions
   [this]
   {"addmbo" #() 
    "fetched-row" #(on-fetched-row this %)
    "reset" (fn [_] (on-reset this))
    "set-control-index" (fn[e]
                          (on-set-control-index this (get e :currrow)))
    "update-mboflag" (fn [e]
                       (let [row (get e :rownum)]
                         (when (= row (get-currow container))
                           (on-set-readonly this (c/str->bool (get e :readonly))))))
    "update-fieldflag" (fn [e] (on-set-fieldflag this (get e :rownum) (get e :field) (get e :flag)))
    "update-control-data"
    (fn[e]
      (let [rownum (get e :rownum)
            column (get e :column)
            value (get e :value)
            ]
        (when
            (= rownum (get-currow container))
          (on-set-max-value this column value))))}
   ))

(def-comp GridInputField [metadata] TextField
  (fn* []
       (this-as this (googbase this metadata)))
  Field
  (get-list-dialog [this listContainer columns]
                   (get-field-list-dialog (-> this get-parent get-parent) this listContainer columns))) ;on this abstract level there is no difference, still good to distinguish

(def-comp CheckBoxGrid [metadata] CheckBox
  (fn* [] (this-as this (googbase this)))
  );same goes here

(def-comp GridRow [container columns mxrow disprow] VisualComponent
  (fn* []
       (this-as this
         (googbase this)
         (c/set-states this
                       {:vcols (seq columns)
                        :deleted false
                        })))
  UI
  (on-set-readonly [this flag]
                   (set-enabled this (not flag)))
  (on-set-deleted [this flag]
                  (c/toggle-state this :deleted flag))
  (add-rendered-child [this rendered-child child] (throw (js/Error. "addRenderedChild not implemented")))
  (draw-row [this] (throw (js/Error. "drawRow not implemented")))
  (draw-field [this] (throw (js/Error. "drawField not implemented")))
  (draw-fields
   [this]
   (c/toggle-state this
                   :column-map
                   (loop [columns (c/get-state this :vcols) colmap {}]
                     (if (empty? columns)
                       colmap
                       (let [col (first columns)
                             _td (draw-field this)
                             col-metadata (c/get-attribute-metadata-with-col-attrs
                                           this
                                           col)
                             row-field (create-field this col-metadata)
                             rendered-field (render row-field)]
                         (add-child this row-field)
                         (add-rendered-child this rendered-field row-field)
                         (recur (rest columns) (assoc colmap col row-field)))))));one level of indirection, in rendering the row, so in case of list we don't render the fields
  (^override render
   [this]
   (let [vr (draw-row this)]
     (c/toggle-state this :rendered vr)
     (draw-fields this )
     vr
     ))
  (set-visible [this visible]
               (c/toggle-state this :visible visible))
  (is-visible [this] (c/get-state this :visible))
  (on-render
   [this]
   (p-deferred-on (c/get-state this :displayed)
                  (listen-row this (fn [_] (selected-action this))))
   )
  (set-enabled
   [this enable]
   (c/toggle-state this :enable enable))
  (is-enabled
   [this]
   (c/get-state this :enable))
  Container
  (get-field-local-value
   [this column]
   (c/get-local-data (c/get-id (c/get-container this)) (get-maximo-row this) column)
   )
  Row
  (is-focused
   [control]
   (throw (js/Error. "isFocused not implemented")))
  (select-row [this selected]
              (let [container (aget this "container")]
                (skip-select-action this true)
                (c/move-to-with-offline (c/get-id container) (get-maximo-row this)
                                        (fn [_]
                                          (c/set-value-with-offline (c/get-id container) "_SELECTED"  (if selected "Y" "N"))))))
  (is-deleted
   [this]
   (c/get-state this :deleted)
   )
  (skip-select-action 
   [this skip]
   (c/toggle-state this :skip-select-action skip)
   )
  (highlight-selected-row
   [this]
   (throw (js/Error. "highlightSelectedRow not implemented")))
  (unhighlight-selected-row
   [this]
   (throw (js/Error. "unhighlightSelectedRow not implemented")))
  (listen-row
   [this rowSelectedActionF]
   (throw (js/Error. "listenRow not implemented")))
  (selected-action 
   [this]
   (row-selected-action (get-parent this) this))
  (set-field-flag [this field flag]
                  (set-flag field flag))
  (get-maximo-row [this]
                  mxrow)
  (get-disp-row
   [this]
   (throw (js/Error. "getDispRow not implemented")))
  (set-disprow!
   [this dr]
   (throw (js/Error. "setDisprow not implemented")))
  (get-field
   [this column]
   (when-let [cm (c/get-state this :column-map)]
     (cm (.toUpperCase column))))
  (get-fields
   [this columnz]
   (loop [ch (get-children this) rez []]
     (if (empty? ch)
       rez
       (let [c (first ch)
             _rez (if (some #(= (.toUpperCase %) (get-column c)) columnz)
                    (conj rez c)
                    rez)]
         (recur (rest ch) _rez)))))
  (set-row-value
   [this column value]
   (set-field-value (get-field this column) value))
  (set-row-values 
   [this colvals]
   (u/debug colvals)
   (doseq [k (keys colvals)]
;;     (u/debug k)
     (set-row-value this k (get colvals k))))
  (set-row-flags
   [this colflags]
   (doseq [k (keys colflags)]
     (set-field-flag this k (get colflags k))))
  (add-default-lookups
   [this columnz]
   (mm/p-deferred-on  (c/get-state this :displayed)
                      (doseq [f (get-fields this columnz)]
                        (add-lookup-internal f))))
  (create-field
   [this col-metadata]
   (let [tp (aget col-metadata "maxType")
         fld  (condp = tp
                "VIRTUAL" (VirtualActionField. col-metadata 0)
                "YORN" (CheckBoxGrid. col-metadata (c/get-id this))
                (GridInputField. col-metadata (c/get-id this)))]
     fld))
  Qbe
  (init-qbe-values [this]
                   (get-qbe container  
                            (fn [qbe]
                              (set-row-values this qbe))
                            (fn [err] (u/debug "Error while qbe init" err))))
  Component
  (get-container
   [this]
   container)
  (get-col-attrs
   [this]
   (c/get-col-attrs (get-parent this)))
  ControlData
  (set-flags-from-local [this]
                        (when-let [flags (c/get-local-flags (c/get-id container) (js/parseInt (get-maximo-row this)))]
                          (doseq [field (keys flags)]
                            (when-let [_f (get-field this field)]
                              (set-flag _f (flags field))))))
  Picker
  (pick [this]);to be called from pickers
  (unpick [this])
  )

(defn get-f-row
  [grid f & guard]
  (let [_mxrows (map #(aget % "mxrow") (get-data-rows grid))
        mxrows (if guard (filter (first guard) _mxrows) _mxrows)]
    (reduce f mxrows)))

                                        ;(defn- get-f-row [grid f & guard]
                                        ;  (when-let [drs (get-data-rows grid)]
                                        ;    (loop [drs drs rez nil]
                                        ;      (if (empty? drs)
                                        ;        rez
                                        ;        (let [mxrow (aget (first drs) "mxrow" )]
                                        ;          (recur (inc i)
                                        ;                 (let [satisfies (if rez (f rez mxrow) mxrow)
                                        ;                       _sat (if guard 
                                        ;                              ((first guard) satisfies)
                                        ;                              true
                                        ;                              )]
                                        ;                   (if _sat satisfies rez))))))))

(defn- get-first-row-bigger-than
  [grid rownum]
  (when-let [fr (get-f-row grid min #(> % rownum))]
    (get-data-row grid fr)))

(defn update-paginator-internal [grid]
  (let [^number crw (get-f-row grid min)
        ^number lrs (get-f-row grid max)
        crwd (if crw (inc crw)  "0")
        lrsd (if lrs (inc lrs)  "0")]
    (if-let  [mbosetcnt (c/get-state grid :mbosetcnt )]
      (update-paginator grid crwd lrsd mbosetcnt)
      (when (c/is-persistent? (-> grid c/get-container c/get-id))
        (c/toggle-state grid :mbosetcnt 0)
        (c/mboset-count-with-offline
         (-> grid c/get-container c/get-id)
         (fn [ev]
           (let [^number crw (get-f-row grid min)
                 ^number lrs (get-f-row grid max)
                 crwd (if crw (inc crw)  "0")
                 lrsd (if lrs (inc lrs)  "0")]
             (update-paginator grid crwd lrsd (first ev))
             (c/toggle-state grid :mbosetcnt (first ev)))))))))

(def-comp Grid [container columns norows] VisualComponent
  (fn* []
       (this-as
           this
         (googbase this)         
         (let [cb-handler (get-callback-handler this)
               err-handler (get-errback-handler this)
               vcols (seq columns)
               deferred (promise-chan)]
           (c/set-states this
                         {:iscontainer false
                          :receiver true
                          :deferred deferred
                          :virtual-deferreds (ar/empty)
                          :norows norows
                          :first-maxrow 0
                          :currrow -1
                          :grid true
                          :columns vcols
                          })
           (c/register-columns container  vcols
                               (fn [ok]
                                 (cb-handler ok)
                                 (go (put! deferred ok)))
                               err-handler)
           (add-child container this))))
  Foundation
  (add-virtual-column
   [this pos column metadata]
   (mm/p-deferred this
                  (let [existing-cols (vec (c/get-state this :columns))]
                    (c/toggle-state this :columns  (-> (subvec existing-cols 0 (min pos (count existing-cols)))  (conj column) (concat (subvec existing-cols (min   pos  (count existing-cols)))) vec))
                    (c/add-virtual-column-to-metadata this (.toUpperCase column)   metadata))))
  UI
  (^override render
   [this]
   (c/set-states this
                 {:table (main-grid-frame this)
                  :header-row (header-row this)
                  :qbe-row (qbe-row-internal this)
                  })
   (p-deferred-on (c/get-state this :displayed)
                  (c/toggle-state this :grid-toolbar (grid-toolbar this))))
  (render-row ;it has the property label row set for the label row
   [this row]
   (throw (js/Error. "renderRow not implemented")))
  (render-row-before
   [this row existing]
   (throw (js/Error. "renderRowBefore not implemented")))
  (on-render
   [this]
   (when (c/get-state this :selectableF)
     (init-data this)))
  (clear-control
   [this]
   (remove-mboset-count this)
   (doseq [nd (get-children this)]
     (remove-child this nd)
     )
   (c/toggle-state this :currrow -1)
   (c/set-states this
                 {:header-row (header-row this)
                  :qbe-row (qbe-row-internal this)
                  :grid-toolbar (grid-toolbar this)}))
  Dialog
  (get-field-list-dialog
   [this field list-container dialog-columns]
   (throw (js/Error. "getFieldListDialog not implemented")))
  Table
  (set-selectableF
   [this selectableF]
   (c/toggle-state this :selectableF selectableF))
  (set-grid-row-value
   [this row column value]
   (set-row-value row column value))
  (add-grid-field-action
   [this row column listenF actionF deferred]
   (add-field-action (get-field row column) listenF actionF deferred)
   )
  (remove-grid-field-action
   [this row column listenF removalF]
   (remove-field-action row (get-field row column) listenF removalF))
  (set-grid-row-values-internal
   [this row values]
   (set-grid-row-values this row values)
   (doseq [k (keys values)]
     (let [dval (get values k)]
       (condp = k
         "readonly" (on-set-grid-row-readonly this row dval)
         "lastRow" (c/toggle-state this :lastRow dval)
         "new" (when  (c/str->bool dval) (remove-mboset-count this))
         "deleted" (on-set-grid-row-deleted this row (c/str->bool dval))
         :true))))
  (set-grid-row-values
   [this row values]
   (set-row-values row values))
  (set-grid-row-flags
   [this row flags]
   (set-row-flags row flags))
  (mark-grid-row-as-selected
   [this row selected]
   ;; (mark-row-as-selected row selected)
   (doseq [r (get-data-rows this)]
     (unhighlight-grid-row this r))
   (if selected
     (highlight-grid-row this row)
     (unhighlight-grid-row this row))
   (mm/loop-arr [d (c/get-state  this :virtual-deferreds)]
                (p/callback d "done"))
   (c/toggle-state this :virtual-deferreds (ar/empty)))
  (highlight-grid-row
   [this row]
   (highlight-selected-row row ))
  (unhighlight-grid-row
   [this row]
   (unhighlight-selected-row row))
  (on-set-grid-row-readonly
   [control row dval]
   (on-set-readonly row dval))
  (on-set-grid-row-deleted
   [control row deleted?]
   (on-set-deleted row deleted?))
  (set-row-visible
   [this row visible]
   (set-visible row visible))
  (qbe-row-internal
   [this]
   (when-let [qr (qbe-row this)]
     (if (c/get-state this :qbe-visible)
       (set-row-visible this qr true)
       (set-row-visible this qr false)
       )
     (init-qbe-values qr)
     qr))
  (get-qbe-row 
   [this]
   (c/get-state this :qbe-row))
  (get-label-row 
   [this]
   (c/get-state this :header-row))
  (get-data-rows
   [this]
   (when-let [children (get-children this)]
     (let [_pom (filter #(aget % "mxrow") children)]
       _pom
       )))
  (get-data-row
   [this rownum]
   (first
    (filter #(when-let [dr (aget % "mxrow")]
               (= rownum dr))
            (get-children this))))
  (main-grid-frame [this] (throw (js/Error. "mainGridFrame not implemented")))
  (header-row [this] (throw (js/Error. "headerRow not implemented")))
  (qbe-row [this] (throw (js/Error. "qbeRow not implemented")))
  (grid-toolbar [this] (throw (js/Error. "gridToolbar not implemented")))
  (remove-mboset-count 
   [this]
   (c/remove-state this :mbosetcnt)
   )
  (table-rows-count
   [this]
   (c/get-state this :mbosetcnt))
  (after-move
   [this f]
   (let [deferred (promise-chan)]
     (c/toggle-state this :moveToDeferred deferred)
     (mm/p-deferred-on deferred (f this))))
  (update-paginator [this fromRow toRow numRows]
                    (throw (js/Error. "updatePaginator not implemented")))
  (clear-data-rows
   [this]
   (remove-mboset-count this)
   (let [_drs (get-data-rows this)]
     (doseq [dr _drs]
       (dispose dr)
       (remove-child this dr))))
  (get-row-control
   [this mxrow disprow]
   (GridRow. container columns mxrow disprow))
  (build-row
   [this rowcontrol]
   (when-let [lookups (aget this "lookupCols")]
     (add-default-lookups rowcontrol lookups))
   (render-deferred rowcontrol)
   (add-child this rowcontrol)
   rowcontrol)
  (append-row [this rowcontrol]
              (let [row (build-row this rowcontrol)]
                (render-row this row );this function decides how is to be appended, if the row is label or qbe or regular
                row))
  (get-new-addrow-num
   [this]
   (if-let [rez  (get-maxrow this (dec (displayed-rows this)))]
     (inc rez)
     0))
  (get-numrows
   [this]
   (c/get-state this :norows))
  (get-firstmaxrow
   [this]
   (c/get-state this :first-maxrow))
  (get-maxrow
   [this disprow]
   (let [dr (get-data-rows this)]
     (when-not (empty? dr)
       (aget (nth dr disprow) "mxrow"))))
  (get-lastrow [this]
               (let [dr (get-data-rows this)]
                 (when-not (empty? dr) 
                   (aget dr (- (count dr) 1)))))
  (get-focused-row
   [this]
   (first (filter #(is-focused %) (get-data-rows this))))
  (set-row-focus [this row]
                 (let [focused (get-focused-row this)]
                   (when-not (= focused row)
                     (mm/c! this "move-to" move-to-row container (get-maximo-row row)))))
  (del-tab-row 
   [this r]
   (when-let [row (first (filter #(= r (get-maximo-row %)) (get-data-rows this)))]
     (remove-child this row)))
  (displayed-rows [this]
                  (count (get-data-rows this)))
  (page-next [this]
             (let [^number mboset-cnt (c/get-state this :mbosetcnt)
                   ^number fmr (get-firstmaxrow this)
                   ^number nrs (get-numrows this)
                   lastpage (when mboset-cnt
                              (>= (+ fmr nrs) mboset-cnt )
                              )]
               (when-not lastpage
                 (when-let [lr (get-label-row this)]
                   (unhighlight-grid-row this lr)
                   )
                 (let [fmr (get-firstmaxrow this)
                       nrs (get-numrows this)
                       lr (+ fmr nrs)]
                   (clear-data-rows this)
                   (c/toggle-state this :first-maxrow lr)
                   (mm/c! this "fetch" fetch-data  container lr nrs)
                   ( move-control-to-row this lr)))))
  (page-prev [this]
             (let [^number fmr (get-firstmaxrow this)
                   ^number nrs (get-numrows this)
                   _lr (- fmr nrs)
                   lr (if (< _lr 0) 0 _lr)
                   ]
               (when-not (= 0 fmr)
                 (let [lr (get-label-row this)]
                   (when-let [lr (get-label-row this)]
                     (unhighlight-grid-row this lr)))
                 (clear-data-rows this)
                 (c/toggle-state this :first-maxrow lr)
                 (mm/c! this "fetch" fetch-data  container lr nrs)
                 (move-control-to-row this lr))))
  (set-numrows [this numrows]
               (let [old-numrows (get-numrows this)
                     first-mr (get-maxrow this 0)
                     ]
                 (c/toggle-state this :norows numrows)
                 (clear-control this)
                 (init-data-from this  first-mr)))
  (add-new-row-to-control
   [this]
   (mm/c! this "fetch" add-new-row-at container (js/parseInt (get-new-addrow-num this)) ))
  (^override move-control-to-row
   [this rownum];TODO probaj ovo da uklopis sa mm/c!, da moze da radi i da poziva cb i errb kao i da moze da se pozove callback direktno
   (c/move-to-with-offline (c/get-id container) rownum
                           (fn[e]
                             (let [row-to (first e)]
                               (c/toggle-state this :highlighted row-to)))))
  (move-control-next [this]
                     (let [curr-row (get-currow container)
                           fmr (get-firstmaxrow this)
                           nrs (get-numrows this)
                           ]
                       (when (= curr-row (+ fmr nrs -1))
                         (c/toggle-state this :first-maxrow (inc fmr))
                         )
                       (move-control-to-row this (inc curr-row))))
  
  (move-control-prev [this]
                     (let [curr-row (get-currow container)
                           fmr (get-firstmaxrow this)
                           ]
                       (when-not (= 0 curr-row)
                         (when (= curr-row fmr)
                           (c/toggle-state this :first-maxrow (dec fmr))
                           )
                         (move-control-to-row this (dec curr-row)))))
  (shift-to [this rownum]
            (let [^number fmr (get-firstmaxrow this)
                  ^number nrs (get-numrows this)
                  ^number _rownum (js/parseInt rownum)]
              (clear-data-rows this)
              (when (>= _rownum (+ fmr nrs))
                (c/toggle-state this :first-maxrow (- _rownum (dec nrs)))
                )
              (when (< _rownum fmr)
                (c/toggle-state this :first-maxrow _rownum)
                )
              (mm/c! this "fetch" fetch-data  container (get-firstmaxrow this) nrs)
              (move-control-to-row this rownum)))
  (fetch-more [this numrows]
              (let [fmr (js/parseInt (get-firstmaxrow this))
                    nrs (js/parseInt (get-numrows this))
                    _numrows (js/parseInt numrows)]
                (c/toggle-state this :norows (+ nrs _numrows))
                (mm/c! this "fetch" fetch-data container (+ fmr nrs) _numrows)))
  (row-selected-action
   [this row-control]
   (when-let [mr (get-maximo-row row-control)]
     (let [selectableF (c/get-state this :selectableF)]
                                        ;the following gives the more responsive grid to the user (if the move to is slow, user may feel the whole maximo as slow)
       (mm/loop-arr [r (get-data-rows this)]
                    (unhighlight-grid-row this r))
       (highlight-grid-row this row-control)

       (c/move-to-with-offline
        (c/get-id container)
        mr
        (fn [ok]
          (when selectableF (after-move this (fn [] (selectableF row-control ok)))))))))
  Row
  (add-default-lookups
   [this columnz]
   (aset this "lookupCols" columnz)
   (mm/loop-arr [dr (get-data-rows this)]
                (add-default-lookups dr columnz))
   (when-let [qbr (get-qbe-row this)]
     (add-default-lookups qbr columnz))
   )
  (add-field-transform 
   [this column transformF]
   (c/add-col-attr this column "custom-transform" transformF)
   (doseq [dr (get-data-rows this)]
     (let [f (get-field dr column)]
       (p-deferred-on (c/get-state f :displayed)
                      (when-let [ff (aget transformF 0)]
                        (ff f))))))
  (remove-field-transform 
   [this column]
   (let [transformF (c/get-column-attribute this column "custom-transform")]
     (mm/loop-arr [dr (get-data-rows this)]
                  (let [f (get-field dr column)]
                    (p-deferred-on (c/get-state f :displayed)
                                   (when-let [ff (aget transformF 1)]
                                     (ff f))))))
   (c/remove-col-attr this column "custom-transform"))
  (add-field-action
   [this column listenF actionF ]
                                        ;ovo treba da obuhvati sve ovo odozdole, tako da 
   (let [deferred (aget this "deferred")]
     (mm/p-deferred-on deferred
                       (let [custom-actions
                             (if-let [cas (c/get-column-attribute this column "custom-actions")]
                               cas
                               (js/Array.)
                               )]
                         (ar/conj! custom-actions (js/Array. listenF  actionF deferred))
                         (c/add-col-attr this column "custom-actions" custom-actions)
                         ))
     (let [drs (get-data-rows this)]
       (doseq [dr drs]
         (add-grid-field-action this dr (get-field dr column) listenF actionF deferred)))))
  (remove-field-action
   [this column actionF removalF]
   (when-let [cas (c/get-column-attribute this column "custom-actions")]
     (let [indF (ar/index-of cas actionF)]
       (when (not= indF -1)
         (ar/remove-at cas indF))))
   (let [drs (get-data-rows this)]
     (mm/loop-arr [dr drs]
                  (remove-grid-field-action this dr (get-field dr column) actionF removalF))))
  (get-fields
   [this columnz]
   (loop [ch (get-children this) rez []]
     (if (empty? ch)
       rez
       (let [c (first ch)
             _rez (if (some #(= (.toUpperCase %) (get-column c)) columnz)
                    (conj rez c)
                    rez)]
         (recur (rest ch) _rez)))))
  MessageProcess
  (on-fetched-row
   [this x]
   (when (:row x)
     (add-row this x))
   (update-paginator-internal this))
  (on-set-fieldflag
   [this row field _flags]
   (let [readonly? (aget _flags 0)
         required? (aget _flags 1)
         ]
     (let [dr (get-data-row this row)
           _fld (when dr (get-field dr field))
           ]
       (when _fld
         (set-enabled _fld (not  readonly?))
         (set-required _fld  required?)))))
  (on-trimmed
   [this x]
   (let [del-rows (fn[this firstrow lastrow]
                    (dotimes [r (- (inc lastrow) firstrow)]
                      (del-tab-row this (+ firstrow r))))]
     (del-rows this (js/parseInt x) (js/parseInt (get-numrows this)))))
  (on-update-control-data
   [this rownum column value]
   (doseq [dr (get-data-rows this)]
     (when (= (js/parseInt  (get-maximo-row dr))  (js/parseInt rownum))
       (set-grid-row-value this dr column value)))) 
  (on-add-at-index
   [this x]
   (let [^number first-maxrow (get-maxrow this 0)
         ^number xint (js/parseInt x)
         data-rows (get-data-rows this)
         ^number previous-disprow (when-let [pr (get-data-row this (dec xint))]
                                    (get-disp-row pr))
         numrows (get-numrows this)
         ]
     (when (and (>= xint first-maxrow) (<= xint (+ first-maxrow (js/parseInt (get-numrows this)))))
       (mm/c! this "fetch" fetch-data  container x (if previous-disprow (- (inc numrows) previous-disprow) numrows)))))
  (on-reset [this]
            (clear-data-rows this)
            (c/toggle-state this :norows norows)
            (init-data this)
            (update-paginator-internal this))
  (on-set-control-index
   [this row]
   (when-not (= -1 row)
     (when-let [mtd (c/get-state this :moveToDeferred)]
       (go (put! mtd row)))
     (let [fmr (get-firstmaxrow this)
           ^number first-maxrow (if (= -1 fmr) 0 fmr)
           norows (get-numrows this)
           last-row (+ first-maxrow (dec norows))
           _row (js/parseInt row)]
       
       (c/toggle-state this :highlighted row)
       (if (and (<= _row last-row) (>= _row first-maxrow))
         (when-not (aget container "fetching")
           (mm/c! this "fetch" fetch-data  container row 1))
         (if (>= _row (+ first-maxrow norows))
           (do
                                        ;             (set-table-control-data (get-id this) #(assoc % :first-maxrow (- row (dec norows))))
             (mm/loop-arr [dr (get-data-rows this)]
                          (when (< (get-maximo-row dr) (- _row (dec norows)))
                            (remove-child this dr)))
             (when-not (aget container "fetching")
               (mm/c! this "fetch" fetch-data  container (inc last-row) (- row (dec last-row)))))
           (do
             (doseq [dr (get-data-rows this)]
               (when (> (get-maximo-row dr) (+ row (dec norows)))
                 (remove-child this dr)))
             (when-not (aget container "fetching")
               (mm/c! this "fetch" fetch-data  container row (- first-maxrow row)))))))))
  Component
  (get-col-attrs [this]
                 (c/get-state this :colAttrs))
  (get-container [this] container)
  Receivable
  (get-receive-functions
   [this]
   {"trimmed" #(do
                 (on-trimmed this (get % :rownum)))
    "fetched-row" (fn [r]
                    (on-fetched-row this r)
                    )
    "fetch-finished" (fn [r]
                       (when (empty? @(c/get-state container :fetch-queue))
                         (on-fetch-finished this)))
    "update-mboflag" (fn [e]
                       (when-let [dr (get-data-row this (get e :rownum))]
                         (on-set-readonly dr (c/str->bool (get e :readonly)))))
    "update-fieldflag" (fn [e] (on-set-fieldflag this (get e :rownum) (get e :field) (get e :flag) ))
    "reset" (fn[_]
              (on-reset this))
    "set-control-index" (fn [e]
                          (on-set-control-index this (get e :currrow)))
    "add-at-and" #(u/debug "add-at-end ")
    "add-at-index" #(on-add-at-index this (get % :rownum))
    "update-control-data" #(on-update-control-data this (get % :rownum)(get % :column) (get % :value))
    "delete" (fn[e]
               (let [data-rows (get-data-rows this)
                     ev-row (get e :rownum)
                     value (get e :value)
                     ]
                 (when-let [dr (get-data-row this ev-row)]
                   (on-set-deleted dr value))))
    })
  ControlData
  (add-row
   [this x]
   (let [data (:data x)
         ^number row (:row x)
         flags (:flags x)
         ks (keys data)
         highlighted-row  (js/parseInt (c/get-state this :highlighted))
         fill-row (fn [disprow]
                    (build-row this
                               (get-row-control this row disprow)
                               ))]
     (when (and row (>= row (get-firstmaxrow this)) (<= row (+ (get-firstmaxrow this) (dec (get-numrows this)))))
       (if-let [first-row (get-maxrow this 0)]
         (do
           (let [row-nodes (get-data-rows this)
                 to-be-updated (get-data-row this row)
                 ]
             (if to-be-updated
               (do
                 (set-grid-row-values-internal this to-be-updated data)
                 (set-grid-row-flags this to-be-updated  flags)
                 (when (= highlighted-row row)
                   (mark-grid-row-as-selected this to-be-updated true)))
               (let [adr (fill-row 0)
                     mr (get-first-row-bigger-than this row) 
                     ]
                 (if mr
                   (render-row-before this adr mr)
                   (render-row this adr))
                 (set-grid-row-values-internal this adr  data)
                 (set-grid-row-flags this adr flags)
                 (when (= highlighted-row row)
                   (mark-grid-row-as-selected this adr true)))))
           (when-let [^number lrn (get-f-row this max)]
             (when (>= lrn (+ (get-firstmaxrow this) (get-numrows this)))

               (del-tab-row this lrn)))
           (when-let [^number frn (get-f-row this min)]
             (when (< frn (get-firstmaxrow this))
               (del-tab-row this frn)))
           (let [row-nodes (get-data-rows this)
                 fmr (get-firstmaxrow this)
                 ]
             (mm/loop-arr [rn row-nodes]
                          (set-disprow! rn (- (get-maximo-row rn) fmr)))))
         (let [adr (fill-row 0)]
           (render-row this adr)
           (set-grid-row-values-internal this adr  data)
           (set-grid-row-flags this adr flags)
           (when (= highlighted-row row)
             (mark-grid-row-as-selected this adr true))
           (when-not (aget data "readonly")
             (set-grid-row-flags this adr flags))
           )))))
  (init-data-from-nd
   [this  start-row]
   (c/set-states this {:first-maxrow start-row
                       :highlighted start-row
                       })
   (mm/c! this "fetch" init-data-with-off  container start-row (get-numrows this)))
  (init-data-from
   [this  start-row]
   (p-deferred this
               (init-data-from-nd this start-row)))
  (init-data
   [this]
   (p-deferred
    this
    (let [cb-handler (get-callback-handler this)
          err-handler (get-errback-handler this)]
      (init-data-with-off container 0 (get-numrows this)
                          (fn [ok]
                            (when cb-handler (cb-handler ok))
                            (c/move-to-with-offline (c/get-id container) 0))
                          (fn [err]
                            (when err-handler (err-handler err)))))))
  Picker
  (pick-row;this will be called just for the pickers
   [this row]
   (pick row))
  (unpick-row
   [this row]
   (unpick row)))

(defn internal-return-selectable-grid
  [dialog listContainer field dialogcols]
  (let [metadata (aget field "metadata")
        column (:attributeName metadata)
        container (c/get-container dialog)]
    (get-selectable-grid 
     dialog
     listContainer
     dialogcols
     (fn[]
       (c/set-value-from-list-with-offline 
        (c/get-id container)
        (c/get-id listContainer) column
        (fn[_] 
          (close-list-dialog dialog)
          (dispose dialog)
          (dispose listContainer)
          )
        (fn [err] ((get-errback-handler dialog)  err)))))))


(defn internal-return-qbe-grid;in the new 1.1 version the dialog will not be closed automatically when it is not clicked on the checkbox
  [dialog listContainer field dialogcols]
  (let [metadata (aget field "metadata")
        column (:attributeName metadata)
        container (c/get-container dialog)
        parentC (get-parent field);qbe section or qbe row
        ]
    (get-selectable-grid 
     dialog
     listContainer
     (cons "_SELECTED" (seq dialogcols)) ;dialogcols
     (fn[]
       (let [_selected (get-field-local-value listContainer "_SELECTED")
             new-sel (if (and _selected (= _selected "Y")) "N" "Y")
             cont-table (aget c/rel-map (c/get-id listContainer))
             table-name (aget c/rel-map (c/get-id container))]
         (if @c/is-offline
           (c/offline-set-value
            (c/get-id listContainer)
            cont-table
            "_SELECTED"
            new-sel
            (fn [_]
              (->
               (off/get-qbe-from-select-list cont-table)
               (p/then
                (fn [qbe]
                  (c/offline-set-qbe
                   (c/get-id container) column qbe
                   (fn [_]
                     (set-row-value parentC column qbe)) nil)))))
            nil)
           (c/set-value  (c/get-id listContainer) "_SELECTED" new-sel 
                         (fn[_] (c/set-qbe-from-list (c/get-id container) (c/get-id listContainer) column 
                                                     (fn[_]
                                                       (init-qbe-values parentC)
                                                       (set-focus field)))))))))))

(mm/def-comp AbstractListDialog [container listContainer field dialogcols] VisualComponent
  (fn* []
       (this-as this (googbase this)))
  Component
  (get-container
   [this]
   container)
  UI
  (^override render
   [this]
   (draw-dialog this)
   (when-let  [list-grid (internal-return-selectable-grid this listContainer field dialogcols)]
     (c/toggle-state list-grid :dialog this)
     (draw-grid-in-dialog this listContainer list-grid)))
  Dialog
  (get-parent-field [this]
                    field)
  (close-action [this]
                (c/unregister-control-with-offline (c/get-id listContainer)
                                                   (fn [e]
                                                     (set-field-focus (get-parent field) field)
                                                     (close-list-dialog this))))
  (draw-dialog [this])
                                        ;when the dialog takes time to render(for example, there is transition going on, we may want to postpone the drawing of grid inside the dialog until it is rendered completely
  (cancel [this]
          (replace-from-local field))
  (draw-grid-in-dialog
   [this listContainer listGrid]
   (throw (js/Error. "drawListInDialog not implemented")))
  (get-selectable-grid
   [this listcontainer dialogcols selectF]
   (throw (js/Error. "getSelectableGrid not implemented")))
  (close-list-dialog [this]
                     (throw (js/Error. "closeListDialog not implemented"))))

(mm/def-comp AbstractQbeListDialog [container listContainer field dialogCols] VisualComponent
  (fn* []
       (this-as this (googbase this)))
  Component
  (get-container
   [this]
   container)
  UI
  (^override render
   [this]
   (when-let [list-grid (internal-return-qbe-grid this listContainer field dialogCols)]
     (c/toggle-state list-grid :dialog this)
     (draw-grid-in-dialog this listContainer list-grid)))
  Dialog
  (draw-dialog [this])
  (cancel [this]
                                        ;          (replace-from-local field)
          )
  (draw-grid-in-dialog
   [this listContainer listGrid]
   (throw (js/Error. "drawListInDialog not implemented")))
  (get-selectable-grid
   [this listcontainer dialogcols selectF]
   (throw (js/Error. "getSelectableGrid not implemented")))
  (close-list-dialog [this]
                     (throw (js/Error. "closeListDialog not implemented")))
  (get-parent-field [this]
                    field)
  (default-action [this]
                  (c/set-qbe-from-list (c/get-id container) (c/get-id listContainer) (get-column field) 
                                       (fn [__] 
                                         (c/unregister-control-with-offline (c/get-id listContainer)
                                                                            (fn [___]
                                                                              (init-qbe-values (.getParent field))
                                                                              (set-focus field)
                                                                              (close-action this))))))
  (close-action [this]
                (c/unregister-control-with-offline (c/get-id listContainer)
                                                   (fn [e]
                                                     (set-field-focus (get-parent field) field)
                                                     (close-list-dialog this)))))



;;the new AbstractPicker will just use Grid for the internal list. Users will override the highlight-selected-row and unhighlihgt-selected-row to display the picked and unpicked row. The only thing left to be done is the error handler of the field, it should get the old value from the local and pick the conforming one from the list
(def-comp AbstractPicker [metadata pickerkeycol pickercol norows] TextField
  (^override fn* []
   (this-as this
     (googbase this metadata)
     (aset this "column" (:attributeName metadata))))
  UI
  (on-render
   [this]
   (display-picker-header this metadata)
   (display-picker-list-internal this))
  Picker
  (get-picker-list
   [this]
   (c/get-state this :picker-list))
  (add-picker-list-internal
   [this picker-list]
   (add-picker-list this picker-list));;this will be overriden in reactive module, so list can be added any time to picker
  (add-picker-list
   [this picker-list]
   (c/set-states this {:picker-list picker-list}))
  (display-picker-header [control metadata]); for overriding
  (build-picker-list [this column listcon pickerkeycol pickercol norows selectableF]
                     (throw (js/Error. "buildPickerList not yet implemented, should return the subclass of AbstractPickerList")))
  (display-picker-list-internal
   [this]
   (let [column (:attributeName metadata)
         container (c/get-container this)
         listcon (ListContainer. container column false)]
     (c/toggle-state this :list-container listcon)
     (mm/p-deferred
      listcon 
      (let [rfn
            (fn[]
              (after-fetch
               listcon
               (fn [ok]
                 (if-let [lv (get-field-local-value listcon (.toUpperCase pickerkeycol))]
                   (do
                     (set-value
                      container
                      column
                      lv
                      (fn[_] 
                        (u/debug "picker list ok")
                        )
                      (fn [err] ((get-errback-handler this)  err))))
                   (u/debug "picker list is null not sure why")))))
            picker-list (build-picker-list this column listcon pickerkeycol pickercol norows rfn)]
        (add-picker-list-internal this picker-list)))))
  (destroy-picker-list
   [this]);;for the components that get a hold on the list to override this
  Field
  (^override set-field-value
   [this value]
                                        ;here it can be assumed that the value is equal to the key in  the picker list. Therefore I will pick the one with the correct key and unpick all others. The implementation may be different on case by case basis
   (aset this "picked" value)
   (if (= value "") ;;clearing of the section
     (changed-row this nil)
     (when-let [picker-list (get-picker-list this)]
       (doseq [dr (get-data-rows picker-list)]
         (let [row-key-value (get-field-local-value dr (.toUpperCase pickerkeycol))]
           (if (= row-key-value value)
             (pick-row picker-list dr)
             (unpick-row picker-list dr)))))))
  (^override changed-row
   [this row]
   ;;I dont know if this is necessary
   (dispose (c/get-state this :list-container))
   (c/remove-state this :list-container)
   (destroy-picker-list this)
   (display-picker-list-internal this)
   )

  )

(mm/def-comp WorkflowCommandContainer [wfControl command args] CommandContainer
  (^override fn* []
   (let [appCont (aget wfControl "appContainer")]
     (this-as this (mm/googbase this appCont command args))))
  App
  (^override errback-handler
   [this error-text error-code error-group error-key]
   (wf-finished wfControl)
   ((get-errback-handler wfControl)[[(keyword error-code) error-text  error-group error-key] nil nil]))
  CommandProcess
  (^override pre-process-command-callback [this e]
   (when-not (c/get-state wfControl :wf-finished)
     (let [act (-> e (get 0) ( get "actions"))
           nextAction (-> e (get 0) (get "nextAction"))
           nextApp (-> e (get 0) (get "nextApp"))
           nextTab (-> e (get 0) (get "nextTab"))
           atInteractionNode (-> e (get 0) (get "atInteractionNode"))
           ]
       (aset this "warnings" (-> e (get 0) ( get "warnings")))
       (aset this "title" (-> e (get 0) ( get "title")))
       (aset this "body"  (-> e (get 0) ( get "body")))
       (if (= "empty" act)
         (do
           (if atInteractionNode
             (if (= "ROUTEWF" nextAction)
               (route-wf wfControl)
               (do
                 (when (or nextApp nextTab nextAction)
                                        ;for some reason when it gets the exception, it goes to the interaction node
                   (handle-interaction wfControl nextApp nextTab nextAction))
                 (set-warnings wfControl (aget this "warnings") (aget this "body") (aget this "title"))))
             (do
               (set-warnings wfControl (aget this "warnings") (aget this "body") (aget this "title"))
               (wf-finished wfControl))) )
         (do 
           (aset this "objectName" (-> e (get 0) (get "actions") (get 1)))
           (set-warnings wfControl (aget this "warnings") (aget this "body") (aget this "title"))))))))

(def-comp WorkflowControl [appContainer processName] VisualComponent
  (fn* []
       (this-as this
         (googbase this)
         (mm/p-deferred
          appContainer
          (c/register-wf-director-with-offline
           (c/get-id appContainer) (aget appContainer "appname") processName (c/get-id this)
           (fn [c]
             (c/is-active-wf-with-offline (c/get-id appContainer)
                                          (fn [c]
                                            (let [active-wf (get c 0)]
                                              (set-wf-active this active-wf)))
                                          (fn [e]
                                            (wf-finished this)
                                            ((get-errback-handler this) e))))
           (fn [e] ((get-errback-handler this) e))))))
  Foundation
  (dispose
   [this]
   (c/unregister-wf-director-with-offline (c/get-id this)))
  Workflow
  (handle-interaction [control nextApp nextTab nextAction]
                      (u/debug "Interaction for app" nextApp " tab " nextTab " and action " nextAction))
  (wf-finished
   [this]
   (c/toggle-state this :wf-finished true)
   (u/debug "workflow control finished"))
  (get-wf-command-container
   [this  command args]
   (WorkflowCommandContainer. this  command args)
   )
  (cancel-wf [this]
             (u/debug "close wf control")
             )
  (set-wf-title
   [this  title]
   (throw (js/Error. "setWfTitle not implemented")))
  (add-wf-action 
   [control f label key]
   (throw (js/Error "addWfAction not implemented"))
   )
  (get-memo-display-container 
   [control]
   (throw (js/Error. "getMemoDisplayContainer not implemented")))
  (get-memo-grid
   [control memocont columns]
   (throw (js/Error "getMemoGrid not implemented")))
  (add-wf-memo-grid
   [control memo-grid]
   (throw (js/Error. "addWFMemoGrid not implemented")))
  (add-wf-memo-grid-internal 
   [control memocont]
   (let [g (get-memo-grid control memocont ["memo" "personid" "transdate"])]
     (add-wf-memo-grid control g)
     (init-wf-memo-grid control g)))
  (init-wf-memo-grid
   [this memo-grid]
   (init-data memo-grid))
  (get-wf-input-fields 
   [control objectName]
   (condp = objectName 
     "COMPLETEWF" ["taskdescription" "actionid" "memo"]
     "REASSIGNWF" ["assignee" "memo"]
     ["actionid" "memo"]
     ))
  (reassign-lookup-columns
   [this]
   ["personid" "displayname" "title" "location" "locationsite" ]
   )
  (draw-wf-input-actions-internal
   [this cont]
   (mm/p-deferred cont 
                  (when-not (c/get-state this :wf-finished)
                    (draw-wf-input-actions this cont)))
   )
  (get-wf-section 
   [this cont objectName secFields]
   (throw (js/Error. "getWfSection not implemented")))
  (add-wf-section
   [this  section]
   (throw (js/Error. "addWfSection not implemented")))
  (draw-wf-input-actions
   [this cont]
   (let [objectName (aget cont "objectName")
         sec-fields (get-wf-input-fields this objectName)
         s (get-wf-section this cont objectName sec-fields)
         ]
     (condp = objectName
       "COMPLETEWF"  (set-wf-title this "Complete Workflow Assignment")
       "INPUTWF" (set-wf-title this  "Manual Input")
       (set-wf-title this  "Reassign")
       )
     (add-wf-action this 
                    (fn [_]
                      (let [add-dialog (if (= objectName "REASSIGNWF") (execute-reassign-wf this)  (choose-wf-action this))]))
                    "OK"
                    "ok"
                    )
     (when (= objectName "COMPLETEWF")    
       (add-wf-action this  
                      (fn [_]
                        (u/debug "Workflow reassign")
                        (reassign-wf this)) "Reassign" "reassign"))
     (add-wf-action this  (fn [_] (cancel-wf this)) "Cancel" "cancel")
     (add-wf-section this  s)
     (init-wf-section this s)
     (when (= objectName "COMPLETEWF") 
       (let [r (RelContainer. cont "MEMOS")]
         (add-wf-memo-grid-internal this  r)))
     nil
     ))
  (init-wf-section
   [this section]
   (init-data section))
  (set-wf-active [this flag]
                 (u/debug "Workflow state for the current mbo is" flag))
  (route-wf [this]
            (c/toggle-state this :wf-finished false)
            (let [cont  (get-wf-command-container this c/route-wf-with-offline [(c/get-id appContainer) (aget appContainer "appname") (c/get-id this) ])]
              (mm/p-deferred cont
                             (draw-wf-input-actions-internal this cont)
                             cont)))
  (reassign-wf [this]
               (let [cont (get-wf-command-container  this c/reassign-wf [(c/get-id this)])]
                 (mm/p-deferred cont
                                (draw-wf-input-actions-internal this cont)
                                cont)))
  (execute-reassign-wf [this]
                       (let [cont (get-wf-command-container this c/execute-reassign-wf [(c/get-id this)])]
                                        ;                      (draw-wf-input-actions-internal this cont)
                         cont))
  (choose-wf-action [this]
                    (let [cont    (get-wf-command-container this c/choose-wf-actions-with-offline [ (c/get-id this)]) ]
                      (mm/p-deferred cont
                                     (draw-wf-input-actions-internal this cont)
                                     cont)))
  (set-warnings
   [control warnings body title]
   (throw (js/Error. "setWarnings not implemented")))
  (prefetch-wf-for-offline 
   [prefetch]
   (->
    (mm/kk-nocb! appContainer "prefetch" c/prefetch-wf-for-offline processName)
    (p/then
     (fn [evt]
       (c/insert-prefetch-offline (c/get-id appContainer) (get-field-local-value appContainer "_uniqueid") evt))))))

(def-comp QbeField [metadata] TextField
  (^overide fn* [] (this-as this (googbase this metadata)))
  UI
  (^override set-required [this flag])
  (^override set-readonly [this flag])
  (^override set-enabled [this flag])
  Field
  (^override get-column
   [this]
   (if-let [virt-name (:virtualName metadata)]
     virt-name
     (:attributeName metadata)))
  (^override local-value [this])
  (^override show-list [this columns]
   (let [ctrl (get-parent this)
         container (aget ctrl "container")
         diag  (get-list-dialog this (ListContainer. container (get-column this) true) columns)
         ]
     (when diag
       (aset diag "parent-field" this))))
  (^override change-maximo-value
   [this value]
   (let  [ctrl (.getParent this)
          container (aget ctrl "container")
          cid (.getId container)
          qbe-prep (:qbePrepend metadata)
          column (if qbe-prep 
                   (str (:virtualName metadata) "/" qbe-prep "/" (:attributeName metadata))
                   (:attributeName metadata))
          _value (if (instance? js/Date value)
                   (c/formatToMaximoDate value)
                   value)
          ]
     (c/set-qbe-with-offline cid column _value
                             (fn[_] (c/get-qbe-with-offline cid
                                                            (fn[e]
                                                              (aset this "qbe" _value)
                                                              (set-field-value this _value))))))))

;;to add the qbePrepend, first init the QbeSection:
;; var q = new QbeSection(cont,["ponum","description"]);
;; then add the prepend columns
;; q.addPrependColumns("from_orderdate", "<=", "orderdate", null);
;;last attribute is the position in the section, null means append at the end
;;then do the q.renderDeferred();
(def-comp QbeSection [container columns] Section
  ;;here there is a special case of putting the objects in columns list
  ;;that will be used for Maximo qbePrepend functionality.
  ;;the format will be {column, qbePrepend}
  (^override fn* []
   (this-as this (googbase this container columns)))
  UI
  (on-render-internal
   [this]
   (init-qbe-values this))
  (^override on-set-max-value [this column value])
  (clear-control
   [this]
   (init-qbe-values this))
  Qbe
  (add-prepend-columns
   [this virtual-name qbe-prepend attribute-name title position]
   (add-virtual-column this position virtual-name #js{"attributeName" attribute-name "qbePrepend" qbe-prepend "virtualName" virtual-name "title" title} ))
  (init-qbe-values 
   [this]
                                        ;for qbesecion the setting is done with full capability of Maximo qbeprepend. Right now that would be very difficult to implement in Offline mode, so I will just copy the simple part from qberow for the offline, becuase that one already works for offline
   (if @c/is-offline
     (get-qbe container  
              (fn [qbe]
                (set-row-values this qbe))
              (fn [err] (u/debug "Error while qbe init" err)))
     (let [fields (get-children this)
           _cols (vec (map (fn [col]
                             (let [ metadata (aget col "metadata")
                                   qbePrepend (:qbePrepend metadata) 
                                   virtualName (:virtualName metadata)
                                        ;id (.getId col)
                                   ]
                               (if qbePrepend
                                 (str virtualName "/" qbePrepend "/" (:attributeName metadata))
                                 (:attributeName metadata))))
                           fields))]
       (c/get-columns-qbe
        (c/get-id container) _cols
        (fn [qbe] 
                                        ;todo map back to qbeprepend field
          (let [_qbe (->> (get qbe 0 )  (apply hash-map))]
            (doseq [ks (keys _qbe)]
              (let [dval (_qbe ks)
                    in (.indexOf ks "/")]
                (when-let [f 
                           (if (not= in -1)
                             (let [id (.substring ks 0 in)]
                               (first (filter #(when-let [vn (:virtualName (aget % "metadata"))]
                                                 (= (.toUpperCase vn ) 
                                                    (.toUpperCase id)))
                                              fields)))
                             (first (filter #(= (.toUpperCase ks) (:attributeName (aget % "metadata")))
                                            fields)))]
                  (set-field-value  f dval))))))))))
  (clear-qbe
   [this]
   (doseq [f (get-children this)]
     (change-maximo-value  f ""))
   (clear-control this)
   (when @c/is-offline
     (->
      (off/get-lists false)
      (p/then (fn [lists]
                (let [table-name (aget c/rel-map (c/get-id (c/get-container this)))]
                  (->
                   (off/remove-selection (filter #(.startsWith % (str "list_" (.toUpperCase table-name)))
                                                 lists))
                   (p/then (fn [_]
                             (off/insert-qbe table-name []))))))))))
  Row
  (^override create-field
   [this col-metadata]
   (QbeField. col-metadata))
  ControlData
  (^override add-row
   [this x])
  (^override init-data
   [this]
   (mm/c! this "getQbe" c/get-qbe-with-offline (c/get-id container)
          (fn[e]
            ))))

(def-comp GLContainer [orgid] MboContainer
  (^override fn* []
   (this-as this
     (.call BaseComponent this);super-super konstruktor
     (c/add-container-to-registry this)
     (let [d (promise-chan)]
       (c/set-states this
                     {:currrow -1
                      :uniqueid (p/get-deferred)
                      :offlineenabled false
                      :iscontainer true
                      :receiver true
                      :rel-containers []
                      :deferred d}
                     )
       (c/register-gl-format
        (c/get-id this) orgid
        (fn [e]
          (c/set-states this
                        {:glformat (first e)
                         :segments (atom (vec (replicate (count (first e)) "")))};;empty vector of segments
                        )
          (c/set-segment (c/get-id this) @(c/get-state this :segments)  0 orgid
                         (fn [ee] (go (put! d ee)))))
        (fn [e] (go (put! d e))))))) 
  GL
  (get-gl-value [this]
                (let [segments @(c/get-state this :segments)
                      seglen (count segments)
                      glformat (c/get-state this :glformat)]
                  (loop [rez "" segs segments glfs (seq glformat)]
                    (let [cur-seg (first segs)
                          cur-glf (first glfs)]
                      (if (empty? segs)
                        (trim rez);;delimiter empty on the right side
                        (recur (str rez 
                                    (let [placeholder (get cur-glf "segment-placeholder")
                                          delimiter (get cur-glf "screen-delimiter")]
                                      (str 
                                       (if (= "" cur-seg)
                                         placeholder
                                         cur-seg)
                                       delimiter)))
                               (rest segs)
                               (rest glfs)))))))
  (set-segment
   [this segmentNo]
   (c/set-segment (c/get-id this)  @(c/get-state this :segments) segmentNo orgid (fn [e] (u/debug e)) (fn [err] (u/debug "error" err)))
   )
  (set-segment-value
   [this segmentNo segmentValue]
   (swap! (c/get-state this :segments) assoc (js/parseInt segmentNo) segmentValue)
   (set-segment this segmentNo)))

(def-comp GLDialog [field orgid] VisualComponent
  (fn* []
       (this-as this
         (googbase this)
         (let [orgid-prom (p/get-deferred)
               cont (c/get-container field)
               dfrd (promise-chan)]
           (c/toggle-state this :deferred dfrd)

           (if-not (c/get-local-data-all-attrs (c/get-id cont) 0)
             ;;there is no container data, can't initialize the control
             (u/debug "No data for the container. Can't initialize the gl dialog.")
             
             (p/then
              (if-not  (empty? orgid) ;;if orgid is supplied by user, use it, otherwise get the orgid from the container first
                (p/get-resolved-promise orgid)
                (let [cad (ComponentAdapter. cont ["orgid"])]
                  (specify! cad
                    UI
                    (on-set-max-value [this column value]
                      (when (= "ORGID" column)
                        (p/callback orgid-prom value)))
                    MessageProcess
                    (on-fetched-row [this row]
                      (let  [val (-> row :data (get "ORGID") )];;debug
                        (p/callback orgid-prom val))))
                  ;; (render-deferred cad) componentadapter not a visual component!
                  (init-data cad)
                  orgid-prom))
              (fn [orgid]
                (c/set-states this
                              {:iscontainer false
                               :receiver true
                               :glcont (GLContainer. orgid)
                               :active-segment (atom 0)
                               })
                (go (put! dfrd "ok"))))))))
  UI
  (^override render
   [this]
   (let [gl-cont (c/get-state this :glcont)
         gl-dialog (get-gl-dialog-holder this 
                                         (fn [_] 
                                           (change-maximo-value field (get-gl-value gl-cont))))
         selectF (fn [row ret]
                   (let [val (c/get-local-data (c/get-id gl-cont) (get-maximo-row row) "COMPVALUE")]
                     (set-segment-value gl-cont @(c/get-state this :active-segment) val)
                     (display-gl-segments this gl-dialog)))]
     (p-deferred gl-cont (display-gl-segments this gl-dialog))
     (display-gl-picker this gl-dialog gl-cont #js ["compvalue" "comptext"] selectF)))
  GL
  (get-gl-value
   [this]
   (get-gl-value (c/get-state this :glcont)))
  (get-account-placeholder
   [control dialog]
   )
  (get-picker-placeholder
   [control dialog]
   (not-implemented))
  (get-gl-dialog-holder
   [this chooseF]
   (not-implemented))
  (clear-gl-segments
   [this dialog]
   (not-implemented))
  (highlight-gl-segment 
   [this dialog segment-no]
   (not-implemented))
  (unhighlight-gl-segments
   [this dialog]
   (not-implemented))
  (display-gl-segment
   [this dialog segmentNo segmentsLength segmentValue segmentName segmentDelimiter active]
   (not-implemented))
  (listen-segment
   [this segment segmentNo  callbackF]
   (not-implemented))
  (display-gl-segments 
   [this dialog]
   (let [glcont (c/get-state this :glcont)]
     (clear-gl-segments this dialog)
     (let [glformat (c/get-state glcont :glformat)
           segments @(c/get-state glcont :segments)
           ln (count segments)
           active @(c/get-state this :active-segment)]
       (dotimes [i ln]
         (let [s (nth segments i)
               empty-s? (= "" s)
               f (nth glformat i)
               placeholder (get f "segment-placeholder")
               delimiter (get f "screen-delimiter")
               name (get f "segment-name")]
           (let [el (display-gl-segment this dialog i ln (if empty-s? placeholder s) name delimiter active)]
             (listen-segment this el i
                             (fn []
                               (reset! (c/get-state this :active-segment ) i)
                               (set-segment glcont i)
                               (unhighlight-gl-segments this dialog)
                               (highlight-gl-segment this dialog i)
                               ) )))))))
  (get-gl-picker-list
   [this  glContainer pickerCols pickedF]
   (not-implemented))
  (add-gl-picker-to-dialog
   [control dialog picker]
   (not-implemented))
  (display-gl-picker [this dialog glContainer pickerCols pickedF] 
                     (let [sel (get-gl-picker-list this glContainer pickerCols pickedF)]
                       (add-gl-picker-to-dialog this dialog sel))))

(defn get-callbacks
  [container command cbh errbh resolver rejecter]
  ;;this will be called from kk! macros -
  ;; it is aimed at simplifying the source as well as generated code
  ;;and reducing the size of compilation
  ;;once this is finished next step is to move internally from promises to channels, and
  ;;keep the promises just as an external javascript interface
  (let [pch (get-prepare-call-handler container)
        fch (get-finish-call-handler container)
        errbh-cont (if errbh errbh (get-errback-handler container))
        cbh-container (if cbh cbh (fn [_]))]
    [pch
     (fn [ok]
       (fch command)
       (cbh-container ok)
       (resolver ok)
       )
     (fn [err]
       (fch command)
       (errbh-cont err)
       (rejecter err))]
    ))

(defn offl [container orig-container level]
  "level 0 are original containers, when the recursion happens level increases. This is used for deletion of temporary containers. We don't want to delete the original containers (level 0)"
  (let [rel-containers (get-rel-containers orig-container)]
    (..
     (get-row-count container nil nil)
     (then 
      (fn [e]
        (let [cnt (get e 0)]
          (.then (fetch-data container 0 cnt nil nil) (fn [_] cnt)))))
     (then
      (fn [cnt]
        (if rel-containers ;fetch from the previos block will put data offline, from here we recursively go to rel containers
          (loop [i 0 rez (p/get-resolved-promise "starting")] 
            (if (>= i cnt)
              rez
              (let [_uid (aget (get-local-data container i) "_uniqueid")
                    uid (.toString _uid)
                    _cnt (UniqueMboContainer. (aget container "mboname") uid)
                    _ (when (not= 0 level) (c/set-offline-enabled-nodel _cnt true )) ;otherwise the rownum will be overwritten in the offline table (unique cont has only row 0)
                    _prm (..
                          rez
                          (then 
                           (fn [_] (c/register-columns _cnt (c/get-registered-columns (c/get-id orig-container)) nil nil)))
                          (then
                           (fn [_] (fetch-data _cnt 0 1 nil nil)))
                          (then
                           (fn [_] (move-to-row _cnt 0 nil nil)))) 
                    fl (fn [r]
                         (let [rel-name (aget r "rel")
                               rcont (RelContainer. _cnt rel-name)
                               _ (c/set-offline-enabled-nodel rcont true)
                               _prm-rel  (..
                                          _prm
                                          (then
                                           (fn[_] (c/register-columns rcont (c/get-registered-columns (c/get-id r)) nil nil))))]
                           (.then _prm-rel (fn [] 

                                             (offl rcont r (inc level))))))
                    rel-promises (map fl rel-containers)]
                (recur (inc i) (..
                                _prm
                                (then
                                 (fn [_] (p/prom-all rel-promises)))
                                (then
                                 (fn [_] 
                                   (.dispose _cnt)
                                   )))))))
          (do
            (when-not (= 0 level)
              (.dispose container)
              )))
        "done")))))

                                        ;TODO kada se merdzuje sa advanced, stavi i ovo da bude u global functions
(defn ^:export notifyOfflineMoveFinished
  [message]
  (js/alert message))

(defn  ^:export toOffline [container]
  (if @c/offline-move-in-progress
    (c/globalErrorHandler "Offline move already in progress" nil nil nil);;TODO i18n
    (do
      (c/set-offline-move-in-progress true)
      (..
       (offl container container 0)
       (then
        (fn []
          (c/set-offline-move-in-progress false)
          (off/mark-as-preloaded (aget c/rel-map (c/get-id container)))))))))

(defn ^:export clearOfflinePreloaded
  [container];;if the preloaded is marked, deletion doesn't remove the records from offline.
  (let [table-name (aget c/rel-map (c/get-id container))
        chl (get-children container)]
    (->
     (off/unmark-as-preloaded table-name)
     (p/then (fn [_] (off/clearTable table-name))))
    (doseq [c chl]
      (clearOfflinePreloaded c))))

(defn get-offline-list-name
  [container col-name]
  (str "list_"
       (.toUpperCase (aget c/rel-map (c/get-id container)))
       "_"
       (.toUpperCase col-name))
  )

(defn ^:export clearOfflinePreloadedList
  ([container col-name]
   (let [table-name (get-offline-list-name container col-name)]
     (->
      (off/unmark-as-preloaded table-name)
      (p/then (fn [_] (off/clearTable table-name)))))))

(declare listToOffline)

(defn ^:export reloadPreloadedList
  [container col-name]
  (->
   (clearOfflinePreloadedList container col-name)
   (p/then
    (fn [_]
      (let [table-name (get-offline-list-name container col-name)]
        (println "gettnit offline meta for list " table-name)
        (off/getObjectMeta table-name))))
   (p/then
    (fn [object-meta]
      (println "object-meta=" object-meta)
      (let [return-column (aget object-meta "returnColumn")
            list-columns (u/read-json (aget object-meta "listColumns"))]
        (listToOffline container col-name list-columns return-column true))))))

(defn ^:export reloadPreloadedLists
  []
  (->
   (off/get-lists true)
   (p/then (fn [res]
             (doseq [l res]
               (let [[_ table column] (split l "_")
                     container-name
                     (aget c/rel-map-reverse
                           (first (filter #(= table (.toUpperCase %)) (js-keys c/rel-map-reverse))))
                     container (@c/registered-components container-name)]
                 (println "container = " container-name " and column =" column)
                 (reloadPreloadedList container column)))))))

(defn ^:export addOfflineListReturnColumn
  [container column value-column]
  (c/add-list-offline-return-column
   (c/get-id container)
   column
   value-column))

(defn ^:export listToOffline
  "value-column is the column which is read from the offline list and set as a value, we have to have it, this is controlled on the server-side while online"
  [container column list-columns value-column force?]
  (let [table-name (aget c/rel-map (c/get-id container))
        list-table-name (str "list_" (.toUpperCase table-name) "_" (.toUpperCase column))]
    (mm/p-deferred 
     container
     (->
      (off/preloaded? list-table-name)
      (p/then
       (fn [preloaded?]
         (when (or (not preloaded?) force?)
           (let [lc (ListContainer. container column false)]
             (c/set-offline-enabled lc true)
             (..
              (c/register-columns lc list-columns nil nil)
              (then
               (fn [_]
                 (off/delete-old-records list-table-name)))
              (then
               (fn [_]
                 (get-row-count lc nil nil)))
              (then
               (fn [e]
                 (let [cnt (get e 0)]
                   (fetch-data lc 0 cnt nil nil))))
              (then
               (fn [_]
                 (c/add-list-offline-return-column (c/get-id lc) value-column)))
              (then
               (fn [_]
                 (c/get-qbe (c/get-id lc) nil nil)));to force writing the offline qbe record for the list
              (then
               (fn [e]
                 (.dispose lc)
                 (off/mark-as-preloaded list-table-name)))
              (then
               (fn [_]
                 (off/updateObjectMeta list-table-name "listColumns" (u/create-json list-columns)))))))))))))

(defn get-unique-ids-container-prom
  [container]
  (..
   (get-row-count container nil nil)
   (then (fn [_cnt]
           (let [cnt (get _cnt 0)]
             (.then (fetch-data container 0 cnt nil nil) (fn [_] cnt)))
           ))
   (then (fn [cnt]
           (let [rez #js[]]
             (loop [i 0]
               (if (>= i cnt)
                 rez
                 (do
                   (ar/conj! rez
                             (->
                              (c/get-local-data (c/get-id container) i "_uniqueid")
                              (.toString)))
                   (recur (inc i) )))))))))

                                        ;this is helper function for looping the main container with unique child containers. Could also be done with macro, but the only benefit would be not passing the function , i.e passing the s-expresssions
(defn loop-unique-conts
  [container unique-ids-prom fnc & enable-offline?]
  (..
   unique-ids-prom
   (then (fn [ids]
           (loop [ids ids loop-promise (p/get-resolved-promise "start") rez []]
             (if (empty? ids)
               (p/prom-all rez)
               (let [prom
                     (.then loop-promise 
                            (fn [_]
                              (let [uid (first ids)
                                    uc (UniqueMboContainer. (aget container "mboname") uid)]
                                (when enable-offline? (c/set-offline-enabled uc true))
                                (..
                                 (mm/p-deferred uc
                                                (c/register-columns uc (c/get-registered-columns (c/get-id container)) nil nil));this function returns promise
                                 (then (fn [_]
                                         (fetch-data  uc 0 1 nil nil)))
                                 (then (fn [_]
                                         ( move-to-row uc  0 nil nil)))
                                 (then (fn [_] (fnc uc)))
                                 (then (fn [rez]
                                         (.dispose uc)
                                         rez))
                                 (thenCatch (fn [err]
                                              (.dispose uc)
                                              err))))))]
                 (recur (rest ids) 
                        prom
                        (conj rez prom)
                        ))))))))

(defn ^:export processFinishedOfflineWFs
  [container wf-process & progress-callback]
  (let [app-name (aget container "appname")
        pof (fn [id]
              (let [uc (UniqueMboContainer. (aget container "mboname") id)]
                (aset uc "errbackHandler" (fn [action err] (u/debug "process finished wf error " err)))
                (..
                 (c/register-columns uc (c/get-registered-columns (c/get-id container)) nil nil);that function returns promise
                 (then (fn [_]
                         (fetch-data  uc 0 1 nil nil)))
                 (then (fn [_]
                         ( move-to-row uc  0 nil nil)))
                 (then (fn [_]
                         (mm/kk-nocb! uc "process" c/replay-wf-from-offline  app-name wf-process true )))
                 (then (fn [rez]
                         (.dispose uc)
                         rez))
                 (thenCatch (fn [err]
                              (.dispose uc)
                              err)))))]
    (..
     (c/get-finished-offline-wfs (c/get-id container))
     (then (fn [_rez]
             (let [rez (map #(aget % "uniqueid") _rez)
                   prom-rez (map pof rez)]
               (p/prom-all prom-rez)))))))


(defn ^:export multiWFPrefetch
  [app-container process-name]
  (loop-unique-conts app-container
                     (get-unique-ids-container-prom app-container)
                     (fn [cont]
                       (..
                        (mm/kk-nocb! cont "prefetch" c/prefetch-wf-for-offline process-name)
                        (then
                         (fn [evt]
                           (c/insert-prefetch-offline (c/get-id cont) (get-unique-id cont)  evt)))))))

