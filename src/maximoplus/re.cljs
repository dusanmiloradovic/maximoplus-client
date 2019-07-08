(ns maximoplus.re
  (:require [maximoplus.core :as c]
            [maximoplus.arrays :as ar]
            [maximoplus.utils :as u]
            [maximoplus.basecontrols :as b :refer [UI Field Row Table ControlData Foundation Dialog Picker Workflow GL MessageProcess]])
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



;;(defn set-wrapped-state
;;  [el property state]
;;  (set-internal-state (aget el "wrapped") property state)
;;  )
;;
;;(defn set-internal-state
;;  [wrapped property state]
;;  (.call (aget wrapped "setInternalState") wrapped property state))

;;For React, getting the state and then act upon it is not guaranteed to be correct. The correct way is to pass the state function. TODO change the template for web components to use the state function

(defn set-internal-state
  [wrapped state-f]
  (.call (aget wrapped "setInternalState") wrapped state-f))

(defn set-wrapped-state
  [el state-f]
  (set-internal-state (aget el "wrapped") state-f))

(defn set-field-state
  [component column type value]
  (set-wrapped-state
   component
   (fn[state]
     (let [ex-state (safe-arr-clone (aget state "maxfields"))
           field-state (-> ex-state (u/first-in-arr #(= column (aget % "column"))))]
       (if field-state
         (do
           (aset field-state type value)
           #js{"maxfields" ex-state})
         #js{})))))

(defn get-field-state
  [component column type]
  (-> component (get-wrapped-state "maxfields")(u/first-in-arr #(= column (aget % "column"))) (aget type)))

(defn remove-field-state
  [component column type]
  (set-wrapped-state
   component
   (fn [state]
     (let [ex-state (safe-arr-clone (aget state "maxfields"))
           field-state (-> ex-state (u/first-in-arr #(= column (aget % "column"))))]
       (js-delete field-state type)
       #js{"maxfields" ex-state}))))

(defn add-field-listener
  [component column type function]
  (set-wrapped-state
   component
   (fn [state]
     (let [ex-state (safe-arr-clone (aget state "maxfields"))
           field-state (-> ex-state (u/first-in-arr #(= column (aget % "column"))))
           field-listeners (aget field-state "listeners")]
       (aset field-listeners type function)
       #js{"maxfields" ex-state}))))

(defn remove-field-listener
  [component column type]
  (set-wrapped-state
   component
   (fn [state]
     (let [ex-state (safe-arr-clone (aget state "maxfields"))
           field-state (-> ex-state (u/first-in-arr #(= column (aget % "column"))))
           field-listeners (aget field-state "listeners")]
       (js-delete field-listeners type)
       #js{"maxfields" ex-state}))))

(defn set-all-fields-state
  [component type state]
  (set-wrapped-state
   component
   (fn [state]
     (let [fields (safe-arr-clone (aget state "maxfields"))]
       (loop-arr [f fields]
                 (aset f type state))
       #js{"maxfields" fields}))))

(defn push-field-state-arr
  [component column type value]
  (set-wrapped-state
   component
   (fn [state]
     (let [field-arr (safe-arr-clone (aget state "maxfields"))
           [field-state ind] (-> field-arr (u/find-in-arr #(= column (aget % "column"))))
           new-field-state (u/clone-object field-state)
           field-arr-state (safe-arr-clone (aget new-field-state type)) ]
       (ar/insert-before!  field-arr-state 0 value)
       (aset new-field-state type field-arr-state)
       (ar/assoc! field-arr ind new-field-state)
       #js{"maxfields" field-arr}))))

(defn pop-field-state-arr
  [component column type]
  (set-wrapped-state
   component
   (fn [state]
     (let [field-arr (safe-arr-clone (aget state "maxfields"))
           [field-state ind] (-> field-arr (u/find-in-arr #(= column (aget % "column"))))
           new-field-state (u/clone-object field-state)
           field-arr-state (safe-arr-clone (aget new-field-state type))]
       (ar/remove-at! field-arr-state 0)
       (aset new-field-state type field-arr-state)
       (ar/assoc! field-arr ind new-field-state)
       #js{"maxfields" field-arr}))))

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
  (set-external-state [this state-f]);;for the performance reason, setstate will not be called during the fetch, we will keep it and send the data when the fetch is finished
  (get-external-state [this property])
  (before-move-externals [this rows])
  (move-externals [this])
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
          :orgid orgid
          })))
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
   (set-wrapped-state
    this
    (fn [state]
      #js{"maxfields" #js[]} ));;initial state
   ) ;wrapper component will draw the frame for fields
  (^override add-rendered-child
   [this rendered-child child]
   (set-wrapped-state
    this
    (fn [state]
      (let [column (b/get-column child)
            new-field-state (get-new-field-state child)
            ex-fields (safe-arr-clone (aget state "maxfields"))]
        (ar/conj! ex-fields new-field-state)
        (b/on-render child)
        #js{"maxfields" ex-fields}))))
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
     (set-field-state this  column "enabled" enabled)
     (set-field-state this column "readonly" (not enabled))))
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
   (set-wrapped-state
    this
    (fn [state]
      #js{"paginator"
       #js{:fromrow fromRow :torow toRow :numrows numRows}})))
  (^override highlight-grid-row
   [this row]
   (set-row-state-meta this row "hightlighed" true))
  (^override unhighlight-grid-row
   [this row]
   (set-row-state-meta this row "hightlighed" false))
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
   (b/remove-mboset-count this)
   (reset! (aget this "children") [])
   (set-external-state
    this
    (fn [state] #js{"maxrows" #js[]}) ))
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
  (before-move-externals
   [this rows]
   (doseq [row rows]
     (let [maximo-row (b/get-data-row this (aget row "mxrow"))]
       (aset row "rowSelectedAction" (fn [_] (b/selected-action maximo-row)) )))
   )
  (move-externals
   [this]
   ;;moves pending to the actual state after the fetching is finished (perfomrance optimization for react)
   (let [st (c/get-state this :re)
         re-state #js{}]
     (doseq [k (js-keys st)]
       (let [v (aget st k )]
         (when (= k "maxrows")
           (before-move-externals this v))
         (aset re-state k (if-not (= k "maxrows")
                            v
                            (ar/cat
                             (if-let [wr-s (get-wrapped-state this "maxrows")]
                               wr-s
                               #js[])
                             v)))))
     (set-wrapped-state
      this
      (fn [state] re-state)))
   (c/remove-state this :re))
  (set-external-state
   [this fn-s]
   (if (c/get-state this :fetching)
     (let [_dl (c/get-state this :re)
           dl (if _dl _dl {})]
       (c/toggle-state this :re (fn-s dl)))
     (set-wrapped-state this fn-s)))
  (set-row-state-data-or-flags
   [this row column type value];;type is data or flag
   (set-external-state
    this
    (fn [state]
      (let [rows-state (aget state "maxrows")
            row-data (-> rows-state (u/first-in-arr #(= (b/get-maximo-row row) (aget % "mxrow"))) (aget type ))] ;;every implementation will have this function
        (aset row-data column value)
        #js{"maxrows" rows-state}))))
  (set-row-state-bulk-data-or-flags
   [this row type _colvals]
   (set-external-state
    this
    (fn [state]
        (let [colvals (u/to-js-obj _colvals)
              mrow (b/get-maximo-row row)
              ;;  maximo-row (b/get-data-row this mrow)
              rows-state (aget state "maxrows")
              rs (-> rows-state
                     (u/first-in-arr
                      #(=  mrow (aget % "mxrow"))))
              row-data (when rs (aget rs type ))]
          (if-not row-data
            (let [ndata (js-obj "mxrow" mrow "data" #js{} "flags" #js{}
                                ;;"rowSelectedAction" (fn [_] (b/selected-action maximo-row))
                                )]
              (aset ndata type colvals)
              (ar/conj! rows-state ndata)
              )
            (if (object-empty? row-data)
              (aset rs type colvals)
              (loop-arr [k (js-keys colvals)]
                        (aset row-data k (aget colvals k)))))
          #js{"maxrows" rows-state}))))
  (set-row-state-meta
   [this row meta value]
   (set-external-state
    this
    (fn [state]
      (let [rows-state (aget state "maxrows")
            rs (-> rows-state (u/first-in-arr #(= (b/get-maximo-row row) (aget % "mxrow"))))]
        (if rs
          (aset rs meta value)
          (let [new-row (js-obj "mxrow" (b/get-maximo-row row) meta value)]
            (ar/conj! rows-state new-row)))
        #js{"maxrows" rows-state}))))
  (remove-row-state-meta
   [this row meta]
   (set-external-state
    this
    (fn [state]
      (let [rows-state  (aget state "maxrows")
            row (-> rows-state (u/first-in-arr #(= (b/get-maximo-row row) (aget % "mxrow"))))]
        (when row
          (js-delete row meta))
        #js{"maxrows" rows-state}))))
  (add-new-row-state-data
   [this row colvals colflags]
   (set-external-state
    this
    (fn [state]
      (let  [_rows-state  (aget state "maxrows")
             rows-state (if _rows-state _rows-state #js[])
             new-maximo-row  (b/get-maximo-row row)
             rows-count (ar/count rows-state)]
        (if (and
             (> 0 rows-count)
             (<= (js/parseInt new-maximo-row) (-> rows-state (aget (- rows-count 1)) (aget "mxrow") (js/parseInt)) ))
          (ar/insert-before! rows-state new-maximo-row #js{:mxrow new-maximo-row :data colvals :flags colflags})
          (ar/conj! rows-state #js{:mxrow new-maximo-row :data colvals :flags colflags} ))
        #js{"maxrows" rows-state}))))
  (del-row-state-data
   [this row]
   (set-external-state
    this
    (fn [state]
      (let  [rows-state  (aget state "maxrows")
             new-maximo-row (b/get-maximo-row row)]
        (when rows-state
          (ar/remove-at! rows-state (u/first-ind-in-arr rows-state #(= (aget % "mxrow") new-maximo-row )))
          #js{"maxrows" rows-state})))))
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
   (set-wrapped-state
    this
    (fn [state]
      #js{"maxfields" #js[]})));;same as for section
  (^override add-rendered-child
   [this rendered-child child]
   (set-wrapped-state
    this
    (fn [state]
      (let [column (b/get-column child)
            new-field-state (get-new-field-state child)
            ex-fields (safe-arr-clone (aget state "maxfields"))]
        (ar/conj! ex-fields new-field-state)
        #js{"maxfields" ex-fields}))))
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
       (b/add-meta this column (keyword k) v))))
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
   [this column value])
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
  (pritln "RN Start"))
