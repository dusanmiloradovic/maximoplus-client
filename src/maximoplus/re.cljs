(ns maximoplus.re
  (:require [maximoplus.core :as c]
            [maximoplus.arrays :as ar]
            [maximoplus.utils :as u]
            [cljs.core.async :as a :refer [put!]]
            [maximoplus.basecontrols :as b :refer [UI Field Row Table ControlData Foundation Dialog Picker Workflow GL MessageProcess]])
  (:require-macros [maximoplus.macros :as mm :refer [def-comp googbase kk! kk-nocb! kk-branch-nocb! p-deferred p-deferred-on react-call with-react-props react-prop react-update-field react-call-control react-loop-fields loop-arr]]
                   [cljs.core.async.macros :refer [go]])
  )

(defprotocol Reactive;;for the 1.1 -React. vue and skatejs components
  ;;will be rendered just by updating the state
  ;;each of these will internally handle the status change differently, but the concepts are the same
  (set-row-state-data-or-flags [this row column type value])
  (set-row-state-bulk-data-or-flags [this row type colvals])
  (set-row-state-meta [this row meta value]);;any metadata on the row that can be interpreted by the implementation. Right now this is just "pick"
  (remove-row-state-meta [this row meta])
  (add-new-row-state-data [this row colvals colflags]);;colvals and colflags are initial flags and values for the row, to avoid re-rendering
  (del-row-state-data [this row])
  (add-columns-meta [this columns-meta]);;shorhand way to add the metadata for the columns
  (get-new-field-state [this]);;this is for columns, each type may give different metadata and state (for example picker lists and text fields)
  (get-external-state [this property])
  (before-move-externals [this rows])
  (move-externals [this])
  (row-action [this mxrownum]);;instead of rowSelectedAction, which results in expensive js closure
  )

(defn not-used;;assure basecomponents don't call the method, not used in reactive. After the extensive testing, all methods having the call to this can be deleted
  []
  (throw (js/Error. "should not be called")))

(defn safe-arr-clone
  [arr]
  (if arr (ar/clone arr) #js[]));;fix for the set-wrapped-state


(defn get-internal-state
  [wrapped property]
  ;;if there is a complex state, sometimes is easier to update property separately. In SkateJS and react, component will be re-rendered, only if the state is different, and the compairson is done on the first level (if the objects are the not identical)
  (.call (aget wrapped "getInternalState") wrapped property))

(defn get-wrapped-state
  [el property]
  (get-internal-state (aget el "wrapped") property))


(defn get-mplus-control
  [react-control]
  (-> react-control (aget "state") (aget "mplus")))
                                        ;there will be no state except on the level of grid and section


(defn set-internal-state
  [wrapped state-f]
  (.call (aget wrapped "setInternalState") wrapped state-f))

(defn set-wrapped-state
  [el state-f]
  (set-internal-state (aget el "wrapped") state-f))

(defn transform-maxrows
  [maxrows]
  (map (fn [k]
         (assoc (get maxrows k) :mxrow k))
       (sort (keys maxrows))))

(defn transform-fields
  [fields]
  (sort-by
   :field-position
   (vals fields)))

(defn set-re-state
  [obj key]
  (let [val (c/get-state obj key)]
    (set-wrapped-state
     obj
     (fn [_]
       (clj->js
        {key
         (case key
           :maxrows (transform-maxrows val)
           :maxfields (transform-fields val)
           val)})))))

(defn schedule-state-update
  [obj key]
  (when-not (c/get-state obj :scheduled-updating)
    (c/toggle-state obj :scheduled-updating true)
    (js/setTimeout
     (fn [_]
       (c/toggle-state obj :scheduled-updating false)
       (set-re-state obj key))
     50)))

;;;GRID

(defn state-row-upsert-helper
  [grid _row f]
  (let [row (b/get-maximo-row _row)
        _rows-state (c/get-state grid :maxrows)
        rows-state (if _rows-state _rows-state {})
        _row-state (get rows-state row)
        row-state (if _row-state _row-state {})
        new-row-state (f row-state)]
    (when (not= new-row-state row-state)
      (let [new-rows-state (assoc rows-state row new-row-state)]
        (c/toggle-state grid :maxrows new-rows-state)
        (schedule-state-update grid :maxrows)))))

(defn state-row-upsert-values
  [grid row type values] ;;type is data or flags
  (state-row-upsert-helper
   grid row
   (fn [row-data]
     (update-in row-data [type] (fn [ex] (merge ex values))))))

(defn state-row-upsert-value
  [grid row type k v]
  (state-row-upsert-helper
   grid row
   (fn [row-data]
     (update-in row-data [type] (fn [ex] (assoc ex k v))))))

(defn state-row-upsert-meta
  [grid row meta value]
  (state-row-upsert-helper
   grid row
   (fn [row-data]
     (assoc row-data meta value))))

(defn state-row-remove-meta
  [grid row meta]
  (state-row-upsert-helper
   grid row
   (fn [row-data]
     (dissoc row-data meta))))

(defn state-row-delete-helper
  [grid _row]
  (when-let [rows-state (c/get-state grid :maxrows)]
    (let [row (b/get-maximo-row _row)
          new-rows-state (dissoc rows-state row)]
      (c/toggle-state grid :maxrows new-rows-state)
      (schedule-state-update grid :maxrows))))

(defn state-clear-rows-helper
  [grid]
  (c/toggle-state grid :maxrows {})
  (schedule-state-update grid :maxrows))

;;;SECTION

(defn state-section-upsert-helper
  [section field f]
  (let [column (b/get-column field)
        _fields-state (c/get-state section :maxfields)
        fields-state (if _fields-state _fields-state {})
        _column-state (get fields-state column)
        column-state (if _column-state _column-state {})
        ex-fields-count (-> fields-state keys count)
        _new-column-state (f column-state)
        new-column-state (if _column-state _new-column-state (assoc _new-column-state :field-position ex-fields-count))]
    (when (not= new-column-state column-state)
      (let [new-fields-state (assoc fields-state column new-column-state)]
        (c/toggle-state section :maxfields new-fields-state)
        (schedule-state-update section :maxfields)))))

(defn state-section-field-state-helper
  [section field _type value]
  (state-section-upsert-helper
   section field
   (fn [field-data]
     (assoc field-data _type value))))

(declare state-section-get-field-state-helper)

(defn state-section-push-field-state-vector-helper
  [section field _type value]
  (let [ex (state-section-get-field-state-helper section field _type)]
    (state-section-field-state-helper section field _type (cons value ex))))

(defn state-section-pop-field-state-vector-helper
  [section field _type]
  (let [ex (state-section-get-field-state-helper section field _type)]
    (state-section-field-state-helper section field _type (rest ex))))

(defn state-section-field-remove-state-helper
  [section field _type]
  (state-section-upsert-helper
   section field
   (fn [field-data]
     (dissoc field-data _type))))

(defn state-section-delete-helper
  [section field]
  (let [column (b/get-column field)
        fields-state (c/get-state section :maxfields)
        new-fields-state (dissoc fields-state column)]
    (c/toggle-state section :maxfields new-fields-state)
    (schedule-state-update section :maxfields)))

(defn state-section-clear-helper
  [section]
  (c/toggle-state section :maxfields {})
  (schedule-state-update section :maxfields))

(defn state-section-new-field-helper
  [section field]
  (let [new-field-state (get-new-field-state field)]
    (state-section-upsert-helper
     section
     field
     (fn [field-data]
       (merge field-data new-field-state)))))

(defn state-section-get-field-state-helper
  [section field _type]
  (let [column (b/get-column field)]
    (-> (c/get-state section :maxfields)
        (get column)
        (get _type))))

(defn state-section-set-all-fields
  [section _type value]
  (when-let [fields-state (c/get-state section :maxfields)]
    (let [new-fields-state (reduce-kv
                            (fn [m k v]
                              (assoc m k (assoc v _type value)))
                            {}
                            fields-state)]
      (c/toggle-state section :maxfields new-fields-state)
      (schedule-state-update section :maxfields))))

(defn add-field-listener
  [component field _type function]
  (let [ex-listeners (state-section-get-field-state-helper component field :listeners)]
    (state-section-field-state-helper component field :listeners (assoc ex-listeners _type function))))

(defn remove-field-listener
  [component field _type]
  (let [ex-listeners (state-section-get-field-state-helper component field :listeners)]
    (state-section-field-state-helper component field :listeners (dissoc ex-listeners _type ))))
;;;

(def-comp ListDialog[container listContainer field dialogcols] b/AbstractListDialog
  (^override fn* [] (this-as this (googbase this container listContainer field dialogcols)))
  Dialog
  (draw-dialog [this])
  (draw-grid-in-dialog [this listContainer listGrid])
  (^override close-list-dialog
   [this]
   (state-section-pop-field-state-vector-helper (b/get-parent field) field :dialogs))
  (^override get-selectable-grid
   [this listcontainer dialogcols selectableF]
   (let [section (b/get-parent field)
         th this
         f field
         dc dialogcols]
     (state-section-push-field-state-vector-helper
      section field :dialogs
      {:type "list"
       :defaultAction selectableF
       :listContainer listcontainer
       :closeAction (fn [_] (b/close-action th))
       :dialogCols dc
       :field f;;in field metadata we will have the list templates beforehand
       :metadata (b/get-metadata f)
       :closeLabel "Cancel"
       })
     nil)))

(def-comp QbeListDialog [container listContainer field dialogcols] b/AbstractQbeListDialog
  (^override fn* [] (this-as this (googbase this container listContainer field dialogcols)))
  Dialog
  (^override draw-dialog [this])
  (^overide draw-grid-in-dialog [this listContainer list-grid])
  (^override close-list-dialog
   [this]
   (state-section-pop-field-state-vector-helper (b/get-parent field) field :dialogs))
  (^override get-selectable-grid
   [this listcontainer dialogcols selectableF]
   (let [section (b/get-parent field)
         th this
         f field
         dc dialogcols]
     (state-section-push-field-state-vector-helper
      section field :dialogs
      {:type "qbelist"
       :defaultAction selectableF
       :listContainer listcontainer
       :closeAction (fn [_] (b/close-action th))
       :dialogCols dc
       :metadata (b/get-metadata f)
       :field f;;in field metadata we will have the list templates beforehand
       :closeLabel "OK"
       })
     nil)))

(def-comp TextField [metadata] b/TextField
  (^override fn* [] (this-as this (googbase this metadata)))
  UI
  (^override render [this]);doesn't do anything, rendering is done in React itself
  (^override add-ui-listener
   [this key f]
   (not-used))
  (^override set-readonly [this readonly]
   (not-used))
  (^override set-required [this required]
   (not-used))
  Field
  (^override add-lookup [this]
   ;;For React add-lookup is not required, because it will be called just when the hasLookup metadata is set, so that is enough info to customize
   )
  (^override set-focus [this]
   (not-used))
  (^override set-field-value [this value]) ;;will be used for picker here noop
  (^override display-label [this value]
   (not-used))
  (^override show-ld-lookup [this]
   (let [section (b/get-parent this)
         column (b/get-column this)
         dac (fn [x]
               (b/set-ld-value this x))
         clac (fn [x]
                (state-section-pop-field-state-vector-helper section this :dialogs)
                (u/debug "Long description lookup closing"))]
     (state-section-push-field-state-vector-helper
      section this :dialogs
      {:type "ld"
          :defaultAction dac
          :closeAction clac})))
  (^override show-date-lookup
   [this]
   (let [section (b/get-parent this)
         dac (fn [x] (b/change-maximo-value this (c/formatToMaximoDate x)))
         clac (fn [x] (state-section-pop-field-state-vector-helper section this :dialogs))]
     (state-section-push-field-state-vector-helper
      section this :dialogs
      {:type "date"
          :defaultAction dac
          :closeAction clac})))
  (^override show-date-time-lookup
   [this]
   (let [section (b/get-parent this)
         dac (fn [x] (b/change-maximo-value this (c/formatToMaximoDate x)))
         clac (fn [x] (state-section-pop-field-state-vector-helper section this :dialogs))]
     (state-section-push-field-state-vector-helper
      section this :dialogs
      {:type "datetime"
       :defaultAction dac
       :closeAction clac})))
  (^override show-gl-lookup
   [this orgid]
   (let [section (b/get-parent this)
         clac (fn [x] (state-section-pop-field-state-vector-helper section this :dialogs))
         that this]
     (state-section-push-field-state-vector-helper
      section this :dialogs
      {:type "gl"
       :field that
       :orgid orgid
       })))
  Reactive
  (get-new-field-state
   [this]
   (let [meta (aget this "metadata")
         column (b/get-column this)
         fld this]
     {:metadata meta
      :column column
      :data nil
      :flags nil
      :readonly true
      :required false
      :listeners  {}
      :focused false
      :maximoField fld
      :dialogs  [];;this is the stack of dialogs, the first on the top should be displayed on top. For the mobile app, that probably means move the stack to the top level of navigator (only one dialog for page). But for the tablet and the desktop app, the dialog can be displayed locally next to the field. Most of the times, there will be only one dialog. When the dialog close is detected, we should call the close dialog functtion, and remove the dialog from the list. If we provide the filters for the dialog, the list will be extended(filter then can have another dialog, and it another filter and so on.., everything is on stack)
      }))
  )

(def-comp CPicker [metadata pickerkeycol pickercol pickerrows] b/AbstractPicker
  (^override fn* []
   (this-as this
     (googbase this metadata pickerkeycol pickercol pickerrows)
     (go (put! (c/get-state this :deferred) "ok"))))
  Reactive
  (get-new-field-state
   [this]
   (let [meta (aget this "metadata")
         column (b/get-column this)
         fld this]
     {:metadata meta
      :column column
      :data nil
      :flags nil
      :readonly true
      :required false
      :listeners  {}
      :picker  {}
      :maximoField fld
      }))
  UI
  (^override render
   [this])
  (^override add-ui-listener [this key f])
  Picker
  (^override display-picker-header
   [this metadata])
  (^override build-picker-list
   [this column list-container pickerkeycol pickercol pickerrows selectableF]
   (let [section (b/get-parent this)
         current-picker-state (state-section-get-field-state-helper section this :picker)]
     (state-section-field-state-helper
      section this
      :picker
      (assoc current-picker-state
             :list {"listContainer" list-container
                    "pickerKeyCol" pickerkeycol
                    "pickerCol" pickercol
                    "maxRows" pickerrows
                    "selectableF" selectableF}))))
  (^override add-picker-list-internal
   [this picker-list])
  ;;by convention the implementing library should put the mp in the state containing the built maximoplus list. I will try to find a better way, maybe pick and unpick is not required
  (^override destroy-picker-list
   [this]
   (let [section (b/get-parent this)]
     (state-section-field-remove-state-helper section this :picker)))
  Field
  (set-field-value
   [this value]
   (let [section (b/get-parent this)
         current-value (state-section-get-field-state-helper section this :data)]
     (when (not= value current-value)
       (b/changed-row this nil)))))

(def-comp Section [container columns] b/Section
  (^override fn* [] (this-as this (googbase this container columns)))
  UI
  (^override draw-section
   [this]
   (state-section-clear-helper this))
  (^override add-rendered-child
   [this rendered-child child]
   (state-section-new-field-helper this child))
  Row
  (^override set-row-field-value
   [this field value]
   (state-section-field-state-helper this field :data value)
   (b/set-field-value field value))
  (^override set-field-enabled
   [this field enabled]
   (state-section-field-state-helper this field :enabled enabled)
   (state-section-field-state-helper this field :readonly (not enabled)))
  (^override set-field-required
   [this field required]
   (state-section-field-state-helper this field :required required))
  (^override create-field
   [this column-metadata]
;;   (println "create-field " column-metadata)
   (let [col-metadata  (if (:isALNDomain column-metadata)
                         (assoc column-metadata :listColumns #js["value" "description"])
                         column-metadata)]
  ;;   (println "col-metadata" col-metadata)
     (if-let [is-picker (and col-metadata (:picker col-metadata))]
       (let [pickerkeycol (:pickerkeycol col-metadata)
             pickercol (:pickercol col-metadata)
             pickerrows (:pickerrows col-metadata)]
         (CPicker. col-metadata  pickerkeycol pickercol pickerrows))
       (TextField. col-metadata))));the types will be controlled from react. The only difference I see is that if the field is a picker, than it is fundamentally different, and has to controlled from here. This will be done by adding to the metadata of the section.
  (^override set-field-flag
   [this field [readonly? required? :as flag]]
   (state-section-field-state-helper this field :flags flag)
   (b/set-field-enabled this field (not readonly?))
   (b/set-field-required this field required?))
  (^override add-field-ui-listeners
   [this field listen-map]
   (doseq [[k v] listen-map]
     (add-field-listener this field k v)))
  (^override set-field-focus
   [this field]
   (when-not (-> field (aget "metadata") :picker)
     (state-section-set-all-fields this :focused false)
     (state-section-field-state-helper this field :focused true)))
  Reactive
  (add-columns-meta
   [this columns-meta]
   (doseq [[column mkv] (js->clj columns-meta)]
     (doseq [[k v]  mkv]
       (b/add-meta this column (keyword k) v))))
  Dialog
  (^override get-field-list-dialog
   [this field list-container dialog-cols]
   (let [d (ListDialog. container list-container field dialog-cols)]
     (c/toggle-state this :list-dialog d)
     (c/toggle-state d :parent-control this)
     (b/render-deferred d)
     d)))

(def-comp GridRow [container columns mxrow disprow] b/GridRow
  (^override fn* [] (this-as this (googbase this container columns mxrow disprow)))
  UI
  (^override draw-row [this]
   (b/mark-as-displayed this);;this will allow the listener to attach(here listener is just state)
   ;;(b/listen-row this (fn [_] (b/selected-action this)))
   );;rendering done in react
  (^override draw-field [this]);rendering done in react
  (^override draw-fields [this]);first version of react will give us just the list, when we are ready to implement the grid, we will call this methid and make the listrow component which inherits this. we don't 
  (^override add-rendered-child [this rendered-child child]);rendering composition should be done in React
  (^override on-render
   [this]
   ;;in the basecontrols we wait until the row is display to attach the listener. For react and similar that will only hurt the performance (one state change more for each row). We will do it immediately in build-row
   )
  Row
  (^override set-row-flags
   [this colfglags]
   (not-used));in the fist version the rows will be read only , so this is not imporant
  (^override highlight-selected-row
   [this]
   (not-used))
  (^override unhighlight-selected-row
   [this]
   (not-used))
  (^override listen-row
   [this rowSelectedActionF]
   (let [list (b/get-parent this)]
     (set-row-state-meta list this "rowSelectedAction" rowSelectedActionF))
   )
  (^override get-disp-row
   [this]
   (not-used))
  (^override set-disprow! [this dr]
   (not-used))
  (^override set-row-value
   [this column value]
   (not-used)
   )
  (^override set-row-values
   [this colvals]
   (not-used)
   )
  (add-default-lookups [this columns])
  (^override set-field-flag
   [this field flag]
   (not-used))
  )




(defn object-empty?
  [obj]
  (or (not obj)
      (= 0 (.-length (js/Object.keys obj)))))




(def-comp Grid [container columns norows] b/Grid ; for the time being full grid will not be done for react
  (^override fn* []
   (this-as this (googbase this container columns norows)))
  Table
  (^override main-grid-frame [this])
  (^override grid-toolbar [this])
  (^override get-qbe-row [this])
  (^override get-label-row [this])
  (^override qbe-row [this])
  (^override header-row [this])
  (^override get-row-control
   [this mxrow disprow]
   (GridRow. container columns mxrow disprow))
  (^override set-grid-row-values
   [this row values]
   (state-row-upsert-values this row :data values))
  (^override set-grid-row-value
   [this row column value]
   (state-row-upsert-value this row :data column value))
  (^override set-grid-row-flags
   [this row flags]
   (when (c/get-state this :fetch-flags)
     (state-row-upsert-values this row :flags flags)))
  (^override mark-grid-row-as-selected
   [this row selected?]
   (set-row-state-meta this row "selected" selected?))
  (^override update-paginator [this fromRow toRow numRows]
   (set-wrapped-state;;???????????????????????TABLE LEVEL META ;;##
    this
    (fn [state]
      #js{"paginator"
       #js{:fromrow fromRow :torow toRow :numrows numRows}})))
  (^override highlight-grid-row
   [this row]
   (set-row-state-meta this row "highlighted" true))
  (^override unhighlight-grid-row
   [this row]
   (set-row-state-meta this row "highlighted" false))
  (fetch-more
   [control num-rows]
   (c/toggle-state control :fetching true))
  (page-next
   [control]
   (c/toggle-state control :fetching true))
  (page-prev
   [control]
   (c/toggle-state control :fetching true))
  (^override clear-data-rows;;check if web components lib is working after this change
   [this]
   (state-clear-rows-helper this) ;;#
   (b/remove-mboset-count this)
   (reset! (aget this "children") []))
  (build-row
   [control rowcontrol]
   ;;in base controls this adds the child to the parent. It is a good place to add a listener property (to avoid setting the state after the render)
;;   (when-not (c/get-state control :fetching)
;;     ;;if multi-fetch, it will be done after the fetch is finished
;;     (b/listen-row rowcontrol
;;                   (fn [_] (b/selected-action rowcontrol))))
   rowcontrol
   )
  MessageProcess
  (on-fetch-finished
   [this]
   (when (c/get-state this :fetching)
     (c/toggle-state this :fetching false)
     (move-externals this)))
  Reactive
  (row-action ;;instead of the closure below
   [this mxrownum]
   (js/requestAnimationFrame
    (fn [_]
      (let [maximo-row (b/get-data-row this mxrownum)]
        (b/selected-action maximo-row)))))
  (before-move-externals
   [this rows])
  (move-externals
   [this]
   ;;moves pending to the actual state after the fetching is finished (perfomrance optimization for react)
   (c/remove-state this :re))
  (set-row-state-meta
   [this row meta value]
   (state-row-upsert-meta this row meta value))
  (remove-row-state-meta
   [this row meta]
   (state-row-remove-meta this row meta))
  (del-row-state-data
   [this row]
   (state-row-delete-helper this row))
  UI
  (^override render-row
   [this row]
   (state-row-upsert-values this row :data {})        ;;##
   (state-row-upsert-values this row :flags {}))
  (^override render-row-before
   [this row existing-row]
   (state-row-upsert-values this row :data {})        ;;##
   (state-row-upsert-values this row :flags {}))
  Foundation
  (^override dispose-child
   [this row]
   (del-row-state-data this row))
  Picker
  (^override pick-row
   [this row]
   (set-row-state-meta this row "picked" true))
  (^override unpick-row
   [this row]
   (set-row-state-meta this row "picked" false))
  ControlData
  (init-data-from-nd
   [control start-row]
   (c/toggle-state control :fetching true))
  (init-data
   [control]
   (c/toggle-state control :fetching true))
   )

(def-comp QbeField [metadata] b/QbeField
  (^override fn* []
   (this-as this (googbase this metadata)))
  UI
  (^override get-ui-event-value [this key ev]
   (react-call-control (b/get-parent this) getEventValue key ev))
  (^override render [this]);doesn't do anything, rendering is done in React itself
  (^override add-ui-listener [this key f]
   (if-let [ui-listeners (c/get-state this "ui-listeners")]
     (c/toggle-state this "ui-listeners" (assoc ui-listeners key f))
     (c/toggle-state this "ui-listeners" {key f})))
  Field
  (^override add-lookup [this])
  (^override set-field-value [this value]
   (b/set-row-field-value (b/get-parent this) this value))
  (^override set-focus [this])
  (^override show-date-lookup
   [this]
   (let [section (b/get-parent this)
         column (b/get-column this)
         dac (fn [x] (b/change-maximo-value this (c/formatToMaximoDate x)))
         clac (fn [x] (state-section-pop-field-state-vector-helper section this :dialogs))]
     (state-section-push-field-state-vector-helper
      section this :dialogs
      {:type "date"
       :defaultAction dac
       :closeAction clac})))
  (^override show-date-time-lookup
   [this]
   (let [section (b/get-parent this)
         column (b/get-column this)
         dac (fn [x] (b/change-maximo-value this (c/formatToMaximoDate x)))
         clac (fn [x] (state-section-pop-field-state-vector-helper section this :dialogs))]
     (state-section-push-field-state-vector-helper
      section this :dialogs
      {:type "datetime"
       :defaultAction dac
       :closeAction clac})))
  Reactive
  (get-new-field-state
   [this]
   (let [meta (aget this "metadata")
         column (b/get-column this)
         fld this]
     {:metadata (u/to-js-obj meta)
         :column column
         :data nil
         :flags nil
         :readonly false
         :required false
         :listeners {}
         :focused false
         :maximoField fld
         :dialogs  [];;this is the stack of dialogs, the first on the top should be displayed on top. For the mobile app, that probably means move the stack to the top level of navigator (only one dialog for page). But for the tablet and the desktop app, the dialog can be displayed locally next to the field. Most of the times, there will be only one dialog. When the dialog close is detected, we should call the close dialog functtion, and remove the dialog from the list. If we provide the filters for the dialog, the list will be extended(filter then can have another dialog, and it another filter and so on.., everything is on stack)
         })))

(def-comp QbeSection [container columns] b/QbeSection
  (^override fn* []
   (this-as this (googbase this container columns)))
  UI
  (^override draw-section
   [this]
   (state-section-clear-helper this))
  (^override add-rendered-child
   [this rendered-child child]
   (state-section-new-field-helper this child))
  Row
  (^override create-field
   [this column-metadata]
   (let  [col-metadata  (if (:isALNDomain column-metadata)
                            (assoc column-metadata :listColumns #js["value" "description"])
                            column-metadata)]
     (QbeField. col-metadata)))
  (^override set-row-field-value
   [this field value]
   (state-section-field-state-helper this field :data value))
  (^override set-field-enabled [this field enabled])
  (^override add-field-ui-listeners
   [this field listen-map]
   (doseq [[k v] listen-map]
     (add-field-listener this field k v)))
  Reactive
  (add-columns-meta
   [this columns-meta]
   (doseq [[column mkv]  (js->clj columns-meta)]
     (doseq [[k v] mkv]
       (b/add-meta this column (keyword k) v))))
  Dialog
  (^override get-field-list-dialog
   [this field list-container dialog-cols]
   (let [d (QbeListDialog. container list-container field dialog-cols)]
     (c/toggle-state this :list-dialog d)
     (c/toggle-state d :parent-control this)
     (b/render-deferred d)
     d)))

(def-comp GLDialog [field orgid] b/GLDialog
  (^override fn* []
   (this-as this (googbase this field orgid)))
  GL
  (^override get-gl-dialog-holder
   [this chooseF]
   (set-wrapped-state
    this
    (fn [state]
      #js{"chooseF" chooseF})))
  (^override display-gl-picker
   [this dialog gl-container picker-cols pickedF]
   (set-wrapped-state
    this
    (fn [state]
      #js{"pickerlist" #js{:glcontainer gl-container :pickercols picker-cols :pickerf pickedF}})))
  (^override display-gl-segment
   [this dialog segmentNo segmentsLength segmentValue segmentName segmentDelimiter active]
   (set-wrapped-state
    this
    (fn [state]
      (let [_ex-segments (aget state "segments")
            ex-segments (if (and _ex-segments (not= 0 (ar/count _ex-segments))) _ex-segments (js/Array. segmentsLength))
            _ex-segment (aget ex-segments (js/parseInt segmentNo))
            ex-segment (if _ex-segment _ex-segment (js-obj))]
        (aset ex-segment "segmentName" segmentName)
        (aset ex-segment "segmentValue" segmentValue)
        (aset ex-segment "segmentDelimiter" segmentDelimiter)
        (aset ex-segments (js/parseInt segmentNo) ex-segment)
        #js{"segments" ex-segments}))))
  (^override get-picker-placeholder
   [control dialog])
  (^override clear-gl-segments
   [this dialog])
  (^override highlight-gl-segment 
   [this dialog segment-no]
   (set-wrapped-state
    this
    (fn [state]
      (let [ex-segments (aget state "segments")
            ex-segment (aget ex-segments (js/parseInt segment-no))]
        (aset ex-segment "highlighted" true)
        #js{"segments" ex-segments}))))
  (^override unhighlight-gl-segments
   [this dialog]
   (set-wrapped-state
    this
    (fn [state]
      (let [ex-segments (aget state "segments")]
        (loop-arr [s ex-segments]
                  (aset s "highlighted" false))
        #js{"segnents" ex-segments}))))
  (^override listen-segment
   [this segment segmentNo  callbackF]
   (set-wrapped-state
    this
    (fn [state]
      (let [ex-segments (aget state "segments")
            ex-segment (aget ex-segments (js/parseInt segmentNo))]
        (aset ex-segment "listener" callbackF)
        #js{"segments" ex-segments}))))
  )

(def-comp WorkflowControl [appContainer processName] b/WorkflowControl
  (^override fn* []
   (this-as this (googbase this appContainer processName)))
  Workflow
  (wf-finished
   [this]
   ;;should just close the workflow control
   (set-wrapped-state this (fn [state] #js{"finished" true}));;this will notify react component to close the wf dialog
   (.call (-> this (aget "wrapped") (aget "closeDialog")) (aget this "wrapped"));;for the react template this will not do anything, closing will be done explicitely from the control itself
   
   )
  (cancel-wf
   [this]
   (set-wrapped-state this (fn [state] #js{"finished"  true}))
   (.call (-> this (aget "wrapped") (aget "closeDialog")) (aget this "wrapped"))
   )
  (^override set-wf-title
   [this title]
   (set-wrapped-state this (fn [state] #js {"title" title "actions" #js{}})))
  (^override add-wf-action
   [this f label key]
   (set-wrapped-state
    this
    (fn [state]
      (let [_ex-actions (aget state "actions")
            ex-actions (if _ex-actions _ex-actions #js{})]
        (aset ex-actions key #js{:actionFunction f :label label})
        #js{"actions" ex-actions}))))
  (^override get-memo-display-container
   [control])
  (^override get-memo-grid
   [this memocont columns]
   (set-wrapped-state
    this
    (fn [state]
      #js{"memo" #js{:container memocont :columns columns}} )))
  (^override add-wf-memo-grid
   [control memo-grid])
  (^override init-wf-memo-grid
   [this memo-grid])
  (^override get-wf-section
   [this cont objectName secFields]
   (set-wrapped-state
    this
    (fn [state]
      #js{"section" #js{:container cont :objectName objectName :fields (clj->js secFields)}})))
  (^override add-wf-section
   [this section]
   ;;this is a good place to indicate that the control is open
   )
  (^override init-wf-section
   [this section])
  (^override set-warnings
   [this warnings errorBody errorTitle]
   (set-wrapped-state
    this
    (fn [state]
      #js{"warnings" (clj->js warnings)
          "error" #js {:errorBody errorBody :errorTitle errorTitle}
          })))
  )

(def-comp ComponentAdapter [container columns norows] b/ComponentAdapter
  (^override fn* []
   (this-as this (googbase this container columns norows)))
  UI
  (clear-control
   [this]
   (set-wrapped-state
    this
    (fn [state] #js{"maxrows" #js[]}) ))
  (on-set-max-value
   [this column value]
   (set-wrapped-state
    this
    (fn [state]
      (let [_rows-state (aget state "maxrows")
            currow (aget state "currow")
            rows-state (if _rows-state _rows-state #js[])
            rs (-> rows-state
                   (u/first-in-arr
                    #(= currow (aget % "mxrow"))))]
        (when rs
          (let [rowdata (aget rs "data")]
            (aset rowdata column value)
            (aset rs "data" rowdata)))
        #js{"maxrows" rows-state}))))
  MessageProcess
  (on-set-control-index
   [this row]
   (set-wrapped-state
    this
    (fn [state]
      #js{"currow" row}
      ))
   )
  (on-fetched-row
   [this row]
   (let [_xrow (:row row)
         data (:data row)
         flags (:flags row)]
     (set-wrapped-state
      this
      (fn [state]
        (let [jsdata (u/to-js-obj data)
              jsflags (u/to-js-obj flags)
              _rows-state (aget state "maxrows")
              rows-state (if _rows-state _rows-state #js[])
              rs (-> rows-state
                     (u/first-in-arr
                      #(= _xrow (aget % "mxrow"))))
              _record #js{"mxrow" _xrow
                          "data" jsdata
                          "flags" jsflags}]
          (if-not rs
            (ar/conj! rows-state _record)
            (do
              (aset rs "data" jsdata)
              (aset rs "flags" jsflags)))
          #js{"maxrows" rows-state})))))
  (on-fetch-finished
   [this])
  )

(defn ^:export rn-start
  []
  (println "RN Start"))
