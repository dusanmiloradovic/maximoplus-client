(ns maximoplus.re
  (:require [maximoplus.core :as c]
            [maximoplus.arrays :as ar]
            [maximoplus.utils :as u]
            [maximoplus.basecontrols :as b :refer [UI Field Row Table ControlData Foundation Dialog Picker Workflow GL]])
  (:require-macros [maximoplus.macros :as mm :refer [def-comp googbase kk! kk-nocb! kk-branch-nocb! p-deferred p-deferred-on react-call with-react-props react-prop react-update-field react-call-control react-loop-fields loop-arr]])
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
  [wrapped property state]
  (.call (aget wrapped "setInternalState") wrapped property state))

(defn set-wrapped-state
  [el property state]
  (set-internal-state (aget el "wrapped") property state)
  )

(defn set-field-state
  [component column type value]
  (let [ex-state (safe-arr-clone (get-wrapped-state component "maxfields"))
        field-state (-> ex-state (u/first-in-arr #(= column (aget % "column"))))]
    (aset field-state type value)
    (set-wrapped-state component "maxfields" ex-state)))

(defn get-field-state
  [component column type]
  (-> component (get-wrapped-state "maxfields")(u/first-in-arr #(= column (aget % "column"))) (aget type)))

(defn remove-field-state
  [component column type]
  (let [ex-state (safe-arr-clone (get-wrapped-state component "maxfields"))
        field-state (-> ex-state (u/first-in-arr #(= column (aget % "column"))))]
    (js-delete field-state type)
    (set-wrapped-state component "maxfields" ex-state)))

(defn add-field-listener
  [component column type function]
  (let [ex-state (safe-arr-clone (get-wrapped-state component "maxfields"))
        field-state (-> ex-state (u/first-in-arr #(= column (aget % "column"))))
        field-listeners (aget field-state "listeners")]
    (aset field-listeners type function)
    (set-wrapped-state component "maxfields" ex-state)))

(defn remove-field-listener
  [component column type]
  (let [ex-state (safe-arr-clone (get-wrapped-state component "maxfields"))
        field-state (-> ex-state (u/first-in-arr #(= column (aget % "column"))))
        field-listeners (aget field-state "listeners")]
    (js-delete field-listeners type)
    (set-wrapped-state component "maxfields" ex-state)))

(defn set-all-fields-state
  [component type state]
  (let [fields (safe-arr-clone (get-wrapped-state component "maxfields"))]
    (loop-arr [f fields]
              (aset f type state))
    (set-wrapped-state component "maxfields" fields)))

(defn push-field-state-arr
  [component column type value]
  (let [field-arr (safe-arr-clone (get-wrapped-state component "maxfields"))
        [field-state ind] (-> field-arr (u/find-in-arr #(= column (aget % "column"))))
        new-field-state (u/clone-object field-state)
        field-arr-state (safe-arr-clone (aget new-field-state type)) ]
    (ar/insert-before!  field-arr-state 0 value)
    (aset new-field-state type field-arr-state)
    (ar/assoc! field-arr ind new-field-state)
    (set-wrapped-state component "maxfields" field-arr)))

(defn pop-field-state-arr
  [component column type]
  (let [field-arr (safe-arr-clone (get-wrapped-state component "maxfields"))
        [field-state ind] (-> field-arr (u/find-in-arr #(= column (aget % "column"))))
        new-field-state (u/clone-object field-state)
        field-arr-state (safe-arr-clone (aget new-field-state type))]
    (ar/remove-at! field-arr-state 0)
    (aset new-field-state type field-arr-state)
    (ar/assoc! field-arr ind new-field-state)
    (set-wrapped-state component "maxfields" field-arr)))

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
  )


(def-comp ListDialog[container listContainer field dialogcols] b/AbstractListDialog
  (^override fn* [] (this-as this (googbase this container listContainer field dialogcols)))
  Dialog
  (draw-dialog [this])
  (draw-grid-in-dialog [this listContainer listGrid])
  (^override close-list-dialog
   [this]
   (pop-field-state-arr (b/get-parent field) (b/get-column field) "dialogs"))
  (^override get-selectable-grid
   [this listcontainer dialogcols selectableF]
   (let [section (b/get-parent field)
         th this
         f field
         dc dialogcols]
     (push-field-state-arr section (b/get-column field) "dialogs"
                           #js{:type "list"
                               :defaultAction selectableF
                               :listContainer listcontainer
                               :closeAction (fn [_] (b/close-action th))
                               :dialogCols dc
                               :field f;;in field metadata we will have the list templates beforehand
                               :closeLabel "Cancel"
                               }))))

(def-comp QbeListDialog [container listContainer field dialogcols] b/AbstractQbeListDialog
  (^override fn* [] (this-as this (googbase this container listContainer field dialogcols)))
  Dialog
  (^override draw-dialog [this])
  (^overide draw-grid-in-dialog [this listContainer list-grid])
  (^override close-list-dialog
   [this]
   (pop-field-state-arr (b/get-parent field) (b/get-column field) "dialogs"))
  (^override get-selectable-grid
   [this listcontainer dialogcols selectableF]
   (let [section (b/get-parent field)
         th this
         f field
         dc dialogcols]
     (push-field-state-arr section (b/get-column field) "dialogs"
                           #js{:type "qbelist"
                               :defaultAction selectableF
                               :listContainer listcontainer
                               :closeAction (fn [_] (b/close-action th))
                               :dialogCols dc
                               :field f;;in field metadata we will have the list templates beforehand
                               :closeLabel "OK"
                               }))))

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
  (^override set-field-value [this value]
   (not-used))
  (^override display-label [this value]
   (not-used))
  (^override show-ld-lookup [this]
   (let [section (b/get-parent this)
         column (b/get-column this)
         dac (fn [x]
               (b/set-ld-value this x))
         clac (fn [x]
                (pop-field-state-arr section column "dialog")
                (u/debug "Long description lookup closing"))]
     (push-field-state-arr
      section
      column
      "dialogs"
      #js{:type "ld"
          :defaultAction dac
          :closeAction clac}))
   )
  (^override show-date-lookup
   [this]
   (let [section (b/get-parent this)
         column (b/get-column this)
         dac (fn [x] (b/change-maximo-value this (c/formatToMaximoDate x)))
         clac (fn [x] (pop-field-state-arr section column "dialog"))]
     (push-field-state-arr
      section
      column
      "dialogs"
      #js{:type "date"
          :defaultAction dac
          :closeAction clac})))
  (^override show-date-time-lookup
   [this]
   (let [section (b/get-parent this)
         column (b/get-column this)
         dac (fn [x] (b/change-maximo-value this (c/formatToMaximoDate x)))
         clac (fn [x] (pop-field-state-arr section column "dialog"))]
     (push-field-state-arr
      section
      column
      "dialogs"
      #js{:type "datetime"
          :defaultAction dac
          :closeAction clac})))
  (^override show-gl-lookup
   [this orgid]
   (let [section (b/get-parent this)
         column (b/get-column this)
         clac (fn [x] (pop-field-state-arr section column "dialog"))
         that this]
     (push-field-state-arr
      section
      column
      "dialogs"
      #js{:type "gl"
          :field that
          :orgid orgid}))
   )
  Reactive
  (get-new-field-state
   [this]
   (let [meta (aget this "metadata")
         column (b/get-column this)
         fld this]
     #js{:metadata (u/to-js-obj meta)
         :column column
         :data nil
         :flags nil
         :readonly true
         :required false
         :listeners #js {}
         :focused false
         :maximoField fld
         :dialogs #js [];;this is the stack of dialogs, the first on the top should be displayed on top. For the mobile app, that probably means move the stack to the top level of navigator (only one dialog for page). But for the tablet and the desktop app, the dialog can be displayed locally next to the field. Most of the times, there will be only one dialog. When the dialog close is detected, we should call the close dialog functtion, and remove the dialog from the list. If we provide the filters for the dialog, the list will be extended(filter then can have another dialog, and it another filter and so on.., everything is on stack)
         }))
  )

(def-comp CPicker [metadata pickerkeycol pickercol pickerrows] b/AbstractPicker
  (^override fn* []
   (this-as this (googbase this metadata pickerkeycol pickercol pickerrows)))
  Reactive
  (get-new-field-state
   [this]
   (let [meta (aget this "metadata")
         column (b/get-column this)
         fld this]
     #js{:metadata (u/to-js-obj meta)
         :column column
         :data nil
         :flags nil
         :readonly true
         :required false
         :listeners #js {}
         :picker #js {}
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
         column (b/get-column this)
         ex-picker (get-field-state section column "picker")]
     (aset ex-picker "list" (clj->js
                             {"listContainer" list-container
                              "pickerKeyCol" pickerkeycol
                              "pickerCol" pickercol
                              "maxRows" pickerrows
                              "selectableF" selectableF}))
     (set-field-state section column "picker" ex-picker)))
  (^override add-picker-list-internal
   [this picker-list])
  ;;by convention the implementing library should put the mp in the state containing the built maximoplus list. I will try to find a better way, maybe pick and unpick is not required
  (^override destroy-picker-list
   [this]
   (let [section (b/get-parent this)
         column (b/get-column this)
         ex-picker (get-field-state section column "picker")]
     (js-delete ex-picker "list")
     (set-field-state section column "picker" ex-picker))))

(def-comp Section [container columns] b/Section
  (^override fn* [] (this-as this (googbase this container columns)))
  UI
  (^override draw-section
   [this]
   (set-wrapped-state this "maxfields" #js[]);;initial state
   ) ;wrapper component will draw the frame for fields
  (^override add-rendered-child
   [this rendered-child child]
   (let [column (b/get-column child)
         new-field-state (get-new-field-state child)
         ex-fields (safe-arr-clone (get-wrapped-state this "maxfields"))]
     (ar/conj! ex-fields new-field-state)
     (set-wrapped-state this "maxfields" ex-fields)))
  Row
  (^override set-row-field-value
   [this field value]
   (if (-> field (aget "metadata") :picker)
     (b/set-field-value field value)
     (this-as this
       (let [column (b/get-column field)]
             (set-field-state this column "data" value)))))
  (^override set-field-enabled
   [this field enabled]
   (let [column (b/get-column field)]
         (set-field-state this  column "enabled" enabled)))
  (^override set-field-required
   [this field required]
   (let [column (b/get-column field)]
     (set-field-state this  column "required" required)))
  (^override create-field
   [this col-metadata]
   (if-let [is-picker (and col-metadata (:picker col-metadata))]
     (let [pickerkeycol (:pickerkeycol col-metadata)
           pickercol (:pickercol col-metadata)
           pickerrows (:pickerrows col-metadata)]
       (CPicker. col-metadata  pickerkeycol pickercol pickerrows))
     (TextField. col-metadata)));the types will be controlled from react. The only difference I see is that if the field is a picker, than it is fundamentally different, and has to controlled from here. This will be done by adding to the metadata of the section.
  (^override set-field-flag
   [this field [readonly? required? :as flag]]
   (set-field-state this (b/get-column field) "flags" (clj->js flag))
   (b/set-field-enabled this field (not readonly?))
   (b/set-field-required this field required?))
  (^override add-field-ui-listeners
   [this field listen-map]
   (doseq [[k v] listen-map]
     (add-field-listener this (b/get-column field) (name k) v)))
  (^override set-field-focus
   [this field]
   (when-not (-> field (aget "metadata") :picker)
     (set-all-fields-state this "focused" false)
     (set-field-state this (b/get-column field) "focused" true)))
  Reactive
  (add-columns-meta
   [this columns-meta]
   (doseq [[column mkv]  (js->clj columns-meta)]
     (doseq [[k v] mkv]
       (b/add-meta this column k v))))
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
   );;rendering done in react
  (^override draw-field [this]);rendering done in react
  (^override draw-fields [this]);first version of react will give us just the list, when we are ready to implement the grid, we will call this methid and make the listrow component which inherits this. we don't 
  (^override add-rendered-child [this rendered-child child]);rendering composition should be done in React
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
   (set-row-state-bulk-data-or-flags this row "data" values ))
  (^override set-grid-row-value
   [this row column value]
   (set-row-state-data-or-flags this row column "data" value))
  (^override set-grid-row-flags
   [this row flags]
   (set-row-state-bulk-data-or-flags this row "flags"  flags))
  (^override mark-grid-row-as-selected
   [this row selected?]
   (set-row-state-meta this row "selected" selected?))
  (^override update-paginator [this fromRow toRow numRows]
   (set-wrapped-state this "paginator" #js{:fromrow fromRow :torow toRow :numrows numRows}))
  (^override highlight-grid-row
   [this row]
   (set-row-state-meta this row "hightlighed" true))
  (^override unhighlight-grid-row
   [this row]
   (set-row-state-meta this row "hightlighed" false))
  Reactive
  (set-row-state-data-or-flags
   [this row column type value];;type is data or flag
   (let [rows-state (safe-arr-clone (get-wrapped-state this "maxrows"))
         row-data (-> rows-state (u/first-in-arr #(= (b/get-maximo-row row) (aget % "mxrow"))) (aget type ))] ;;every implementation will have this function
     (aset row-data column value)
     (set-wrapped-state this "maxrows" rows-state)))
  (set-row-state-bulk-data-or-flags
   [this row type _colvals]
   (let [colvals (u/to-js-obj _colvals)
         mrow (b/get-maximo-row row)
         rows-state  (safe-arr-clone (get-wrapped-state this "maxrows"))
         rs (-> rows-state
                (u/first-in-arr
                 #(=  mrow (aget % "mxrow"))))
         row-data (when rs (aget rs type ))]
     (if-not row-data
       (let [ndata (js-obj "mxrow" mrow "data" #js{} "flags" #js{})]
         (aset ndata type colvals)
         (ar/conj! rows-state ndata))
       (if (object-empty? row-data)
         (aset rs type colvals)
         (loop-arr [k (js-keys colvals)]
                   (aset row-data k (aget colvals k)))))
     (set-wrapped-state this "maxrows" rows-state)))
  (set-row-state-meta
   [this row meta value]
   (let [rows-state (safe-arr-clone (get-wrapped-state this "maxrows"))
         row (-> rows-state (u/first-in-arr #(= (b/get-maximo-row row) (aget % "mxrow"))))]
     (aset row meta value)
     (set-wrapped-state this "maxrows" rows-state)))
  (remove-row-state-meta
   [this row meta]
   (let [rows-state  (safe-arr-clone (get-wrapped-state this "maxrows"))
         row (-> rows-state (u/first-in-arr #(= (b/get-maximo-row row) (aget % "mxrow"))))]
     (js-delete row meta)
     (set-wrapped-state this "maxrows" rows-state)))
  (add-new-row-state-data
   [this row colvals colflags]
   (let  [rows-state  (safe-arr-clone (get-wrapped-state this "maxrows"))
          new-maximo-row  (b/get-maximo-row row)
          rows-count (ar/count rows-state)]
     (if (and
          (> 0 rows-count)
          (<= (js/parseInt new-maximo-row) (-> rows-state (aget (- rows-count 1)) (aget "mxrow") (js/parseInt)) ))
       (ar/insert-before! rows-state new-maximo-row #js{:mxrow new-maximo-row :data colvals :flags colflags})
       (ar/conj! rows-state #js{:mxrow new-maximo-row :data colvals :flags colflags} ))
     (set-wrapped-state this "maxrows" rows-state)))
  (del-row-state-data
   [this row]
   (let  [
          rows-state (safe-arr-clone (get-wrapped-state this "maxrows"))
          new-maximo-row (b/get-maximo-row row)]
     (when rows-state
       (ar/remove-at! rows-state (u/first-ind-in-arr rows-state #(= (aget % "mxrow") new-maximo-row )))
       (set-wrapped-state this "maxrows" rows-state))))
  UI
  (^override render-row
   [this row]
   (add-new-row-state-data this row #js{} #js{}))
  (^override render-row-before
   [this row existing-row]
   (add-new-row-state-data this row #js{} #js{});same as before becuase the functions rearanges the rows based on the mxrow
   )
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
   (remove-row-state-meta this row "picked"))
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
         clac (fn [x] (pop-field-state-arr section column "dialog"))]
     (push-field-state-arr
      section
      column
      "dialogs"
      #js{:type "date"
          :defaultAction dac
          :closeAction clac})))
  (^override show-date-time-lookup
   [this]
   (let [section (b/get-parent this)
         column (b/get-column this)
         dac (fn [x] (b/change-maximo-value this (c/formatToMaximoDate x)))
         clac (fn [x] (pop-field-state-arr section column "dialog"))]
     (push-field-state-arr
      section
      column
      "dialogs"
      #js{:type "datetime"
          :defaultAction dac
          :closeAction clac})))
  Reactive
  (get-new-field-state
   [this]
   (let [meta (aget this "metadata")
         column (b/get-column this)
         fld this]
     #js{:metadata (u/to-js-obj meta)
         :column column
         :data nil
         :flags nil
         :readonly false
         :required false
         :listeners #js {}
         :focused false
         :maximoField fld
         :dialogs #js [];;this is the stack of dialogs, the first on the top should be displayed on top. For the mobile app, that probably means move the stack to the top level of navigator (only one dialog for page). But for the tablet and the desktop app, the dialog can be displayed locally next to the field. Most of the times, there will be only one dialog. When the dialog close is detected, we should call the close dialog functtion, and remove the dialog from the list. If we provide the filters for the dialog, the list will be extended(filter then can have another dialog, and it another filter and so on.., everything is on stack)
         })))

(def-comp QbeSection [container columns] b/QbeSection
  (^override fn* []
   (this-as this (googbase this container columns)))
  UI
  (^override draw-section
   [this]
   (set-wrapped-state this "maxfields" #js[]));;same as for section
  (^override add-rendered-child
   [this rendered-child child]
   (let [column (b/get-column child)
         new-field-state (get-new-field-state child)
         ex-fields (safe-arr-clone (get-wrapped-state this "maxfields"))]
     (ar/conj! ex-fields new-field-state)
     (set-wrapped-state this "maxfields" ex-fields)))
  Row
  (^override create-field
   [this col-metadata]
   (QbeField. col-metadata)
   )
  (^override set-row-field-value
   [this field value]
   (set-field-state this (b/get-column field) "data" value))
  (^override set-field-enabled [this field enabled])
  (^override add-field-ui-listeners
   [this field listen-map]
   (doseq [[k v] listen-map]
     (add-field-listener this (b/get-column field) (name k) v)))
  Reactive
  (add-columns-meta
   [this columns-meta]
   (doseq [[column mkv]  (js->clj columns-meta)]
     (doseq [[k v] mkv]
       (b/add-meta this column k v))))
  Dialog
  (^override get-field-list-dialog
   [this field list-container dialog-cols]
   (let [d (QbeListDialog. container list-container field dialog-cols)]
     (c/toggle-state this :list-dialog d)
     (c/toggle-state d :parent-control this)
     (b/render-deferred d)
     d))
  )

(def-comp GLDialog [field orgid] b/GLDialog
  (^override fn* []
   (this-as this (googbase this field orgid)))
  GL
  (^override get-gl-dialog-holder
   [this chooseF]
   (set-wrapped-state this "chooseF" chooseF))
  (^override display-gl-picker
   [this dialog gl-container picker-cols pickedF]
   (set-wrapped-state this "pickerlist" #js{:glcontainer gl-container :pickercols picker-cols :pickerf pickedF}))
  (^override display-gl-segment
   [this dialog segmentNo segmentsLength segmentValue segmentName segmentDelimiter active]
   (let [_ex-segments (get-wrapped-state this "segments")
         ex-segments (if (and _ex-segments (not= 0 (ar/count _ex-segments))) _ex-segments (js/Array. segmentsLength))
         _ex-segment (aget ex-segments (js/parseInt segmentNo))
         ex-segment (if _ex-segment _ex-segment (js-obj))]
     (aset ex-segment "segmentName" segmentName)
     (aset ex-segment "segmentValue" segmentValue)
     (aset ex-segment "segmentDelimiter" segmentDelimiter)
     (aset ex-segments (js/parseInt segmentNo) ex-segment)
     (set-wrapped-state this "segments" ex-segments)))
  (^override get-picker-placeholder
   [control dialog])
  (^override clear-gl-segments
   [this dialog])
  (^override highlight-gl-segment 
   [this dialog segment-no]
   (let [ex-segments (get-wrapped-state this "segments")
         ex-segment (aget ex-segments (js/parseInt segment-no))]
     (aset ex-segment "highlighted" true)
     (set-wrapped-state this "segments" ex-segments)))
  (^override unhighlight-gl-segments
   [this dialog]
   (let [ex-segments (get-wrapped-state this "segments")]
     (loop-arr [s ex-segments]
               (aset s "highlighted" false))
     (set-wrapped-state this "segments" ex-segments)))
  (^override listen-segment
   [this segment segmentNo  callbackF]
   (let [ex-segments (get-wrapped-state this "segments")
         ex-segment (aget ex-segments (js/parseInt segmentNo))]
     (aset ex-segment "listener" callbackF)
     (set-wrapped-state this "segments" ex-segments)))
  )

(def-comp WorkflowControl [appContainer processName] b/WorkflowControl
  (^override fn* []
   (this-as this (googbase this appContainer processName)))
  Workflow
  (wf-finished
   [this]
   ;;should just close the workflow control
   (.call (-> this (aget "wrapped") (aget "closeDialog")) (aget this "wrapped"))
   )
  (cancel-wf
   [this]
   (.call (-> this (aget "wrapped") (aget "closeDialog")) (aget this "wrapped"))
   )
  (^override set-wf-title
   [this title]
   (set-wrapped-state this "title" title)
   (set-wrapped-state this "actions" #js{})
   )
  (^override add-wf-action
   [this f label key]
   (let [_ex-actions (get-wrapped-state this "actions")
         ex-actions (if _ex-actions _ex-actions #js{})]
     (aset ex-actions key #js{:actionFunction f :label label})
     (set-wrapped-state this "actions" ex-actions)))
  (^override get-memo-display-container
   [control])
  (^override get-memo-grid
   [this memocont columns]
   (set-wrapped-state this "memo" #js{:container memocont :columns columns}))
  (^override add-wf-memo-grid
   [control memo-grid])
  (^override init-wf-memo-grid
   [this memo-grid])
  (^override get-wf-section
   [this cont objectName secFields]
   (set-wrapped-state this "section" #js{:container cont :objectName objectName :fields (clj->js secFields)}))
  (^override add-wf-section
   [this section]
   ;;this is a good place to indicate that the control is open
   )
  (^override init-wf-section
   [this section])
  (^override set-warnings
   [this warnings errorBody errorTitle]
   (set-wrapped-state this "warnings" warnings)
   (set-wrapped-state this "error" #js {:errorBody errorBody :errorTitle errorTitle}))
  )
