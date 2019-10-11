(ns maximoplus.offline
  (:require 
   [maximoplus.utils :as u]
   [maximoplus.arrays :as ar]
   [maximoplus.promises :as p]
   [maximoplus.db :as db :refer [ddl dml]]
   [maximoplus.net :as net :refer [get-tabsess]]
   )
  (:use [clojure.string :only [join replace split blank?]]
        [clojure.set :only [difference]])
  (:require-macros 
   [maximoplus.macros :as mm :refer [do-offline]]
   [maximoplus.db :refer [dml1]]))

(declare globalFunctions)

(def offline-enabled
  (p/get-deferred);it has to be deferred, becuase we trigger it from the outside
  )

(def object-promises (atom {}))

(defn preloaded?
  [table-name]
  (db/preloaded? table-name))

(defn enable-offline
  []
  (when-not (p/has-fired? offline-enabled)
    ;;(db/setEngine (db/get-default-engine))
    (p/callback offline-enabled "enabled")))

(declare get-object-meta-deferred)
(declare prepare-one-flags)

(defn database-ensure-open
  []
  (->
   (db/exist-object? "objectMeta")
   (p/then (fn [ex?]
             (when-not ex?
               (ddl [{:type :create :name "objectMeta" :key "objectName" :keyType "text"}
                     {:type :create :name "objectQbe" :key "objectName" :keyType "text"}
                     {:type :create :name "workflowPrefetch" :key "uniqueid"}
                     {:type :create :name "offlineChanges" :key "changeId"}]))))))

(def db-ready
  (->
   offline-enabled
   (p/then
    (fn [_]
      (database-ensure-open)))))

(defn db-ready?
  []
  db-ready)

(defn get-object-meta
  [object-name]
  (db/get-object-meta object-name))

(defn get-column-names
  [object-name]
  (db/get-column-names object-name))



(defn construct-object-meta
  "no need to construct object meta now, no sql, straight insert into in"
  [rel-name])

(defn exist-table?
  [table-name & raw?]
  (db/exist-object? table-name ))

;;TODO low priority repalce json with transit

(defn merge-meta
  [ex-meta new-meta]
  (let [_ex-meta (js->clj ex-meta :keywordize-keys true)]
    (loop [nm new-meta rez _ex-meta]
      (if  (empty? nm)
        rez
        (let [ma (first nm)
              attribute-name (:attributeName ma)
              ex-already?  (filter (fn [x]
                                   (= attribute-name (:attributeName x)))
                                  ex-meta)]
          (recur (rest nm) (if-not (empty? ex-already?)
                             rez
                             (conj rez ma))))))))

(defn moveMeta
  [object-name object-meta]
  ;;  (.log js/console (str "!!!!!!!!!! Move meta for " object-name))
;;  (println "moveMeta" object-name " meta:" object-meta)
  (let [prom (p/get-deferred)]
    (swap! object-promises assoc object-name prom)
    (do-offline
     (fn [_]
       ;;       (dml
       ;;        [{:type :select-by-key :name "objectMeta" :key object-name :key-name "objectName" }] true true))
       (db/select {:key object-name
                   :key-name "objectName"
                   :name "objectMeta"}))
     (fn [res]
;;       (println "--->>>>>>>> select meta " res)
       (let [existing (first res)]
         (->
          (ddl
           [{:type :create :name object-name :key "uniqueid" :index-columns [#js["parentid" "rownum"]] :columns-meta object-meta}
            {:type :create :name (str object-name "_flags") :key "uniqueid" :index-columns [#js["parentid" "rownum"]] :columns-meta object-meta}])
          (p/then (fn []
                    (if-not existing
                      (do
  ;;                      (println "creating " object-name " from " object-meta)
                        (dml
                         [{:type :put :name "objectMeta" :data #js {"objectName" object-name "columnsMeta" object-meta}}] true))
                      (let [merged-meta (merge-meta (aget (first res) "columnsMeta")
                                                    object-meta)]
;;                        (println "merged meta " merged-meta)
                        (db/update {:key object-name
                                    :name "objectMeta"
                                    :qbe {"objectName" object-name}
                                    :key-name "objectName"
                                    :update (fn [meta]
                                              (aset meta "columnsMeta" (clj->js merged-meta))
                                              meta) }))))))))
     (fn [_] 
       ;;       (.log js/console (str  "meta move has finished for" object-name))
       (when-not (p/has-fired? prom )
         ;;         (.log js/console "firing object promise!!!!!!!!!")
         (p/callback prom))))))

(defn column-in-meta?
  [control-meta column]
  (if (or (= column "parentid") (= column "uniqueid")  (= column "changed"))
    true
    (loop [cm control-meta]
      (when-not (= 0 (.-length cm))
        (if (= column (-> cm (aget 0) (aget "attributeName")))
          true
          (recur (ar/remove-at cm 0)))))))

(defn clearTable
  [control-name]
  ;;  (.log js/console (str "calling the clear table for" control-name))
;;  (println "calling clearTable " control-name)
  (dml [{:type :delete :name control-name}]))



(defn moveToOfflineInternal
  [table-name control-data]
;;  (println "moving to offline" table-name)
  (when (and control-data (get control-data "uniqueid"))
    (do-offline
;;     (fn [_]
;;       (.log js/console (str "********starting moving to offline " table-name))
;;       (.log js/console control-data))
     (fn [_]
       (@object-promises table-name)
       )
     (fn [_]
         (dml
        [{:type :put :name table-name :data (if-not (get control-data "parentid")
                                              (assoc control-data "parentid" -1 "tabsess" (get-tabsess))
                                              (assoc control-data "tabsess"(get-tabsess)))}]
        ))
;;     (fn [_]
;;       (.log js/console (str "********ending moving to offline " table-name)))
     )))

(defn normalize-data
  [obj]
  (into
   {}
   (for [[k v] obj]
     [k (if-not (sequential? v)
          v
          (u/transit-json v))])))

(defn ^:export moveToOffline
  [control-name control-data]
  (moveToOfflineInternal control-name control-data))


(defn ^:export moveFlagsToOffline
  [control-name control-data]
  (moveToOfflineInternal (str control-name "_flags") (normalize-data control-data)))


(defn ^:export table-count
  "we need the parentid as a parameter becuase this simulates the original maximo data"
  [table-name parentid]
  (do-offline
   (fn [_] (db/get-offline-qbe-where table-name parentid) )
   (fn [condition]
     (dml
      [(merge {:type :select :name table-name} condition)] true true))
   (fn [res]
     (.-length (aget res 0)))))

(defn ^:export debug-table
  [table-name]
;;  (.log js/console (str "Debug rable " table-name))
  (..
   (dml [{:type :select :name table-name}])
   (then (fn [res]
           (.log js/console res)))))

(defn prepare-to-delete
  [table-name]
  ;;when the init data is done on the object, the changes may be posted to Maximo, and then the new data is inserted. It may happen that we have duplicate data (old and new) after this. This function designates row to be deleted by deleting its rownum
  (->
   (db/exist-object? table-name)
   (p/then
    (fn [ex?]
      (when ex?
        (dml [{:type :update :name table-name :updateObject {"rownum" -1}}]))))))


(defn delete-old-records
  [table-name]
  ;;once the offline posting is finished, delete the old records
  ;;unless it is marked as preloaded
;;  (println "Calling delete-old-records " table-name)
  (->
   (db/exist-object? table-name)
   (p/then
    (fn [ex?]
      (when ex?
        (->
         (preloaded? (.toLowerCase table-name))
         (p/then
          (fn [_preloaded?]
            (when-not _preloaded?
              (dml [{:type :delete :name table-name
                     :qbe {"tabsess" ["!=" (get-tabsess)]}}
                    {:type :delete :name (str table-name "_flags")
                     :qbe {"tabsess" ["!=" (get-tabsess)]}}]))))))))))

(defn insert-qbe
  [table-name _qbe]
  (let [qbe (clj->js (u/vec-to-map _qbe))]
    (do-offline
     (fn [_] (db/select {:name "objectQbe" :key table-name :key-name "objectName"}))
     (fn [res]
       (if (empty? res)
         (dml [{:type :put :name "objectQbe" :data #js {"objectName" table-name "qbe" qbe}}] true )
         (db/update {:name "objectQbe" :key table-name :key-name "objectName"
                     :update (fn[x]
                               (aset x "qbe" qbe) x) }))))))

(defn insert-order-by
  [table-name orderby]
  (->
   (db/select {:name "objectQbe" :key table-name :key-name "objectName"})
   (p/then (fn [res]
           (if (empty? res)
             (dml [{:type :put :name "objectQbe" :data #js {"objectName" table-name "orderby" orderby}}] true )
             (db/update {:name "objectQbe" :key table-name :key-name "objectName"
                         :update (fn[x]
                                   (aset x "orderby" orderby) x) }))))))

(defn insert-offline-qbe
  "insert the qbe in offline mode- limited functionality for offline search"
  [table-name column-name qbe]
  (->
   (db/select {:name "objectQbe" :key table-name :key-name "objectName"})
   (p/then (fn [res]
             (let [exis-qbe (if-let [exs-offlineqbe  (aget (first res) "qbe")]
                              exs-offlineqbe
                              #js {}
                              )]
               (aset exis-qbe (.toUpperCase column-name) qbe)
               (if (empty? res)
                 (dml [{:type :put :name "objectQbe" :data #js {"objectName" table-name "qbe" exis-qbe}}] true )
                 (db/update {:name "objectQbe" :key table-name :key-name "objectName" :update (fn[x] (aset x "qbe" exis-qbe) x)} )))))))

(defn get-qbe
  "this is read in offline mode"
  [table-name]
  (->
   (db/select {:name "objectQbe" :key table-name :key-name "objectName"})
   (p/then
    (fn [res]
      (if (empty? res)
        []
        (let [qbeo (aget (first res) "qbe")
              qbe (->> qbeo js->clj (into []) (apply concat) vec)]
          [qbe]
          ))))))

(defn get-order-by
  [table-name]
  (->
   (db/select {:name "objectQbe" :key table-name :key-name "objectName"})
   (p/then (fn [res]
           (if (empty? res)
             ""
             (-> res first (aget "orderby"))
             )))))

(defn row-del-undel
  [table-name parent-id rownum del-flag]
  (->
   (db/get-unique-id table-name parent-id rownum)
   (p/then (fn [uniqueid]
           [uniqueid (dml1 {:type :select-by-key :name table-name :key uniqueid :key-name "uniqueid"} true true)]))
   (p/then (fn [[uniqueid val]]
           (let [_cv (db/get-changed-value val)
                 cv (if _cv _cv #js{})]
             (aset cv "_deleted" del-flag)
             (db/set-changed-value val cv)
             (aset val "deleted" del-flag)
             (dml1 {:type :update-by-key :name table-name
                    :key uniqueid
                    :key-name "uniqueid"
                    :updateObject {"changed" true
                                   "deleted" del-flag
                                   "changedValue" (db/convert-changed-value cv)}
                    :update (fn [x] val)}))))))

(defn row-delete
  [table-name parent-id rownum]
  (row-del-undel table-name parent-id rownum true))

(defn row-undelete
  [table-name parent-id rownum]
  (row-del-undel table-name parent-id rownum false))


(defn set-value
  [table-name parent-id rownum attribute value]
  (do-offline
   (fn [_] (db/get-unique-id table-name parent-id rownum))
   (fn [id]
     (->
      (dml [{:type :select-by-key :name table-name :key id :key-name "uniqueid"}]);;For sqlite, we have first to select
      (p/then (fn [[rez]]
              (let [_cv (-> rez first db/get-changed-value)
                    cv (if _cv (do (aset _cv attribute value) _cv) (clj->js {attribute value}))
                    update-object {"changedValue" (db/convert-changed-value cv)
                                   attribute value}]
                (dml [{:type :update-by-key :name table-name
                       :key id :key-name "uniqueid"
                       :updateObject update-object
                       :update (fn [x]
                                 (aset x "changed" true)
                                 (aset x attribute value)
                                 (db/set-changed-value x cv)
                                 x)}]))))))))





(defn fetch-multi-rows [table-name parent-id start-row numrows]
  (let [;condition (if-not parent-id (fn[x] true) #(= parent-id (aget % "parentid")))
                                        ; condition (get-offline-qbe-where table-name parent-id)
        _sr (js/parseInt start-row)
        _nr (js/parseInt numrows)]
    (do-offline
     (fn [_] (db/get-offline-qbe-where table-name parent-id) )
     (fn [condition]
       ;;       (println "For table " table-name " got the qbe condition " condition)
       (->
        (dml1 (merge
               {:type :select :name table-name :start _sr :rows _nr :order-by "rownum" :index-column #js["parentid" "rownum"]}
               condition) true true );;TODO order by in the offline mode (ascending and descending). Rownum is necessary for sqlite
        (p/then (fn [dta]
                  (if (empty? dta)
                    [#js[] #js[]]
                    (let [uniqueids (map (fn [x] (aget x "uniqueid")) dta)
                          wff-flags (fn [x] (some (fn [y] (= (aget x "uniqueid") y)) uniqueids))]
                      (->
                       (dml1 {:type :select :name (str table-name "_flags") :where wff-flags :qbe {"uniqueid" ["=" uniqueids]} :start 0 :rows _nr} true true)
                       (p/then (fn [flgs]
                                 [dta flgs])))))))))
     (fn [[dta flgs]]
       (let [dta-len (.-length dta)]
         (loop [i 0 rez []]
           (if (= i dta-len)
             rez
             (let [dta-obj (aget dta i)
                   flags-obj (aget flgs i)
                   uniqueid (aget dta-obj "uniqueid")
                   dta-keys (js-keys dta-obj)]
               (recur (inc i)
                      (conj rez
                            [(+ _sr i)
                             (loop [obj-rez {} dta-keys dta-keys]
                               (if (empty? dta-keys)
                                 obj-rez
                                 (recur
                                  (let [k (first dta-keys)]
                                    (if (or (= k "uniqueid") (= k "parentid")(= k "changed") (= k "rownum") (= k "new") (= k "readonly") (= k "changedValue") )
                                      (assoc obj-rez (replace k "_dot_" ".")   (aget dta-obj k))
                                      (let [flg (u/transit-read
                                                 (aget flags-obj k))
                                            required? (when flg (get flg 1))
                                            readonly? (when flg (get flg 0))
                                            dkey (replace k "_dot_" ".")
                                            dval (aget dta-obj k)
                                            compval [dval
                                                     (if required? 8
                                                         (if readonly? 7 0))] ]
                                        (assoc obj-rez dkey compval))))
                                  (rest dta-keys))))]))))))))))

;;for some reason, the following two block the promise if the do-offline is used. Investigate! In the meantime, no curr_oper promise is set for them
(defn get-ids-for-parent
  [rel-name parent-id & raw?]
  (do-offline
   (fn [_]
     (dml [{:type :select
            :name rel-name
            :qbe {"parentid" ["=" parent-id]}}] true true))
   (fn [rez]
     (let [_len (.-length rez)]
       (loop [i 0 vec []]
         (if (>= i _len)
           vec
           (recur (inc i) (conj vec (-> rez (aget i) (aget "uniqueid"))))))))))


(defn delete-for-parent
  [rel-name parent-id & raw?]
  ;;  (.log js/console (str "*****delete for parent " rel-name " and id " parent-id))
;;  (println "calling delete for paretn for " rel-name " and parent id " parent-id)
  (let [del-qbe (when parent-id
                  {"uniqueid" ["=" parent-id]})
        del-deferred (p/get-deferred)
        del-statement {:type :delete :name rel-name}
        del-statement-flags {:type :delete :name (str rel-name "_flags")}]
    (do-offline 
     (fn [_]
       ;;(.log js/console (str "******starting delete for parent for " rel-name))
       (@object-promises rel-name))
     (fn [_]
       (swap! object-promises assoc rel-name del-deferred))
     (fn [_] (db/exist-object? rel-name))
     (fn [ok]
       (if ok
         (dml [(if del-qbe (assoc del-statement :qbe del-qbe) del-statement)
               (if del-qbe (assoc del-statement-flags :qbe del-qbe) del-statement-flags)] true)
         (p/get-resolved-promise "no table")))
     (fn [_]
       ;;(.log js/console (str "******ending delete for parent for " rel-name))
       (p/callback del-deferred)))))


                                        ;move the function from core.cljs to here because it has to be atomic, otherwise inserts that come after delete may be deleted
(defn delete-for-parent-full
  [control-name rel]
  )

(defn updateObjectMeta
  "for keeping other metadata other than column meta"
  [table-name attribute value]
  (db/update
   {:key table-name :name "objectMeta" :key-name "objectName"
    :update (fn [obj]
              (if (map? value)
                (doseq [[k v] value]
                  (aset obj k v))
                (aset obj attribute value)) obj)}))

(defn getObjectMeta
  [table-name]
  (first
   (db/select {:name "objectMeta" :key-name "objectName" :key table-name})))

(defn getReturnListValue
  [list-table-name rownum]
  (db/get-return-list-value list-table-name rownum))

(defn insertCoreWFMeta
  "the point is that we can't register the inputwf, completewf without affecting the workflow. this function fakes their offline registration, so the offline routing of wf becomes possible"
  [metadata]
  (let [iw-meta (aget metadata 0)
        cw-meta (aget metadata 1)
        actions-meta (aget metadata 2)]
    (ar/remove-at! iw-meta 0)
    (ar/remove-at! cw-meta 0)
    (ar/remove-at! actions-meta 0)
    (->
     (moveMeta "INPUTWF" iw-meta)
     (p/then (fn [_]
             (moveMeta "COMPLETEWF" cw-meta)))
     (p/then (fn [_]
             (moveMeta "list_INPUTWF_ACTIONID" actions-meta)))
     (p/then (fn [_]
             (moveMeta "list_COMPLETEWF_ACTIONID" actions-meta))))))

(defn insertOfflineWF
  [table-name  rownum workflow-data]
  (do-offline
   (fn [_] (db/get-unique-id table-name nil rownum));parentid=nil , just the main object
   (fn [uniqueid] (dml1 {:type :put  :name "workflowPrefetch" :data #js {:objectName table-name :uniqueid uniqueid :offlineSteps workflow-data}} true))))

(defn insert-offline-wf-with-id
  [table-name uniqueid workflow-data]
  "when we do prefetch, we loop against the unique containers. During the looping the foffline is switch off for unique containers in order not to overwrite data for the main (actual) container. Therefore the rownum will be 0 as it is the unique container, and we will lose the fetch for other records"
  (dml1 {:type :put  :name "workflowPrefetch" :data #js {:objectName table-name :uniqueid uniqueid :offlineSteps workflow-data}} true)
  )

(defn get-curr-node-actions
  "traverse the tree using the prevoiusly selected actions by the user, to find the current wf node and actions"
  [node steps]
  (if (empty? steps)
    node
    (let [actionid  (-> steps first first str)
          actions (second node)
          rest-tree (nth node 2)
          nn (->
              (filter (fn [x] (= actionid (aget x "actionid"))) actions )
              first
              )
          next-node-id (when nn (aget nn "membernodeid"))
          next-node (->>
                     rest-tree
                     (filter (fn [n] (= next-node-id (aget (aget n 0) "nodeid"))) )
                     first
                     )
          ]
      (when next-node
        (recur next-node (rest steps))))))

(defn update-wf-tables
  [node-type node]
  (->
   (dml [{:type :delete :name node-type :qbe nil}
         {:type :delete :name (str node-type "_flags") :qbe nil}
         {:type :delete :name (str "list_" node-type "_ACTIONID") :qbe nil}
         {:type :delete :name (str "list_" node-type "_ACTIONID_flags") :qbe nil }])
   (p/then (fn [_]
           (let [actions (second node)
                 default-actionid (-> actions (aget 0) (aget "actionid"))
                 _actions 
                 (loop [cntr 0 actions actions rez []]
                   (if (empty? actions)
                     rez
                     (let [a (first actions)]
                       (aset a "rownum" cntr)
                       (aset a "uniqueid" cntr)
                       (recur (inc cntr) (rest actions) (conj rez a)))))]
             (if (= node-type "INPUTWF")
               (dml [{:type :put :name "INPUTWF" :data #js {:uniqueid "np" :parentid -1 :rownum 0 :ACTIONID default-actionid :MEMO ""} }
                     {:type :put :name "INPUTWF_flags" :data #js {:uniqueid "np" :parentid -1 :rownum 0 :ACTIONID #js [true false] :MEMO #js [false false] }}] true )
               (dml [{:type :put :name "COMPLETEWF" :data #js {:uniqueid "np" :parentid -1 :rownum 0 :ACTIONID default-actionid :TASKDESCRIPTION (aget (aget node 0) "description") :MEMO ""}}
                     {:type :put :name "COMPLETEWF_flags" :data #js {:uniqueid "np" :parentid -1 :rownum 0 :ACTIONID #js [false false] :TASKDESCRIPTION #js [true false] :MEMO #js [false false]}}] true))
             (let [list-data  (map (fn [action] 
                                     {:type :put :name (str "list_" node-type "_ACTIONID") :data #js {:uniqueid (aget action "uniqueid") :parentid -1 :rownum (aget action "rownum") :ACTIONID (aget action "actionid") :INSTRUCTION (aget action "instruction")}}) _actions)
                   list-flags  (map (fn [action]
                                      {:type :put :name (str "list_" node-type "_ACTIONID_flags") :data #js {:uniqueid (aget action "uniqueid") :parentid -1 :rownum (aget action "rownum") :ACTIONID #js [false false] :INSTRUCTION #js [false false]}}) _actions)]
               (dml (concat list-data list-flags) true)))))))

(defn handleErrorMessage
  [msg]
  (.call (aget globalFunctions "handleOfflineErrorMessage" ) nil msg))


(defn handle-error-message [msg] (js/alert msg))

(def ^:export globalFunctions
  #js {"handleNoWFData" (fn [] (handleErrorMessage "No offline workflow data loaded for the record"))
       "handleWFAlreadyFinished" (fn [] (handleErrorMessage "Offline workflow already finished, it will continue when online"))
       "handleWFContinueOnOnline" (fn [] (handleErrorMessage "Workflow process will cotinue when online"))
       "handleWFOnStopNode" (fn [] (handleErrorMessage "Offline workflow completed, reached stop node"))
       "handleOfflineErrorMessage" handle-error-message
       "defaultDBEngine" db/get-default-engine-name
       }
  
  )



(defn- handle-no-wf-data
  []
  ((aget globalFunctions "handleNoWFData")))

(defn- handle-wf-already-finished []
  ((aget globalFunctions "handleWFAlreadyFinished")))

(defn- handle-wf-continue-on-online []
  ((aget globalFunctions "handleWFContinueOnOnline")))

(defn- handle-wf-on-stop-node []
  ((aget globalFunctions "handleWFOnStopNode")))



(defn routeWF;one can route just the main object so the parentid is always nil
  [table-name rownum]
  (->
   (db/get-unique-id table-name nil rownum)
   (p/then (fn [uniqueid]
           (let [wff(fn [x] (and
                             (= table-name (aget x "objectName"))
                             (= (str uniqueid) (aget x "uniqueid"))))]
             (->
              (dml1 {:type :select :name "workflowPrefetch" :where wff} true)
              (p/then    (fn [rez]
                         (if (empty? rez)
                           (do
                             (handle-no-wf-data)
                             #js [#js {"actions" "empty"
                                       "atInteractionNode" false
                                       "warnings" nil
                                       }])
                           (let [pf (aget rez 0)
                                 steps (aget pf "stepsFinished")
                                 top-node-act (get-curr-node-actions (aget pf "offlineSteps") steps)
                                 status (aget pf "status")]
                             (if (= "FINISHED" status)
                               (do
                                 (handle-wf-already-finished)
                                 #js [#js {"actions" "empty"
                                           "atInteractionNode" false
                                           "warnings" nil
                                           }])
                               (if top-node-act
                                 (if (= "STOP"  (aget (aget top-node-act 0) "type"));!CHECK if this is correct condition
                                   (do
                                     (handle-wf-on-stop-node)
                                     (->
                                      (db/update {:name "workflowPrefetch" :where (fn [x] (= uniqueid (aget x "uniqueid")))  :update (fn [o] (aset o "status" "FINISHED") o) })
                                      (p/then (fn [_]
                                              #js [#js {"actions" "empty"
                                                        "atInteractionNode" false
                                                        "warnings" nil
                                                        }]))))
                                   (let [table-name (if (= "INPUT" (aget (aget top-node-act 0) "type"))
                                                      "INPUTWF"
                                                      "COMPLETEWF")]
                                     (->
                                      (update-wf-tables table-name top-node-act)
                                      (p/then (fn [_]
                                              (db/update {:name "workflowPrefetch" :where wff
                                                          :update
                                                          (fn [o]
                                                            (aset o "STATUS" table-name)
                                                            o
                                                            )})))
                                      (p/then (fn [_]
                                              #js [#js {"actions" #js ["none" table-name]
                                                        "atInteractionNode" false
                                                        }])))
                                     ))
                                 (do
                                   (if steps
                                     (handle-wf-continue-on-online)
                                     (handle-no-wf-data))
                                   #js [#js {"actions" "empty"
                                             "atInteractionNode" false
                                             "warnings" nil
                                             }]
                                   )))))))))))))

(defn chooseWFAction
  [table-name rownum]
  (..
   (db/get-unique-id table-name nil rownum)
   (p/then (fn [uniqueid]
           (..
            (dml1 {:type :select :where (fn [x] (= (str uniqueid) (aget x "uniqueid")))  :name "workflowPrefetch" } true)
            (p/then 
             (fn [rez]
               (let [action-table-name (aget (aget rez 0) "STATUS")
                     steps (aget (aget rez 0) "stepsFinished")]
                 (->
                  (dml1 {:type :select-all :name action-table-name } true true)
                  (p/then (fn [actiona]
                          (let [_obj (aget actiona 0)
                                actionid (aget _obj "ACTIONID")
                                memo (aget _obj "MEMO")
                                new-steps (if-not steps #js [#js [actionid memo]] (ar/conj steps #js [actionid memo]))]
                            (->
                             (db/update {:name "workflowPrefetch" :key uniqueid :key-name "uniqueid"
                                         :update (fn [o] (aset o "stepsFinished" new-steps) o)})
                             (p/then (fn [rez]
                                     (routeWF table-name rownum)))
                             (p/then (fn [ret]
                                     (if (= "empty" (-> ret (aget 0) (aget "actions")))
                                       (->
                                        (db/update {:name "workflowPrefetch" :key uniqueid :key-name "uniqueid"
                                                    :update (fn [o] (aset o "status" "FINISHED") o)})
                                        (p/then (fn [_] ret)))
                                       ret))))))))))))))))

(defn getCompletedWFActions
  [table-name finished? uniqueid]
  (->
   (dml1 {:type :select :name "workflowPrefetch"
          :where (fn [o]
                   (and
                    (= (aget o "objectName") table-name)
                    (if finished? (= (aget o "status") "FINISHED") true)
                    (= (js/parseInt (aget o "uniqueid")) uniqueid)))} true true)
   (p/then (fn [rez]
           (-> rez (aget 0) (aget "stepsFinished"))))))

;;TODO introduce qbe
(defn deleteCompletedWFAction
  [table-name uniqueid]
  (dml1 {:type :delete :name "workflowPrefetch"
         :where (fn [o]
                  (and
                   (= (aget o "objectName") table-name)
                   (= (js/parseInt (aget o "uniqueid")) uniqueid)))} true false))

                                        ;TODO test the case when we have the prefetch offline data, and the control goes offline and then again online. I have to get the uniqueid of the chosen task in the online mode and update the finished steps offline

(defn getFinishedOfflineWFs
  [table-name]
  (dml1 {:type :select :name "workflowPrefetch"
         :where (fn [o]
                  (and
                   (= "FINISHED" (aget o "status"))
                   (= table-name (aget o "objectName")))) } true true))

(defn- group-by-parent-id
  [values]
  (loop [res {} values values]
    (let [val (first values)]
      (if-not val
        res
        (recur
         (let [parid (aget val "parentid")
               exis (res parid )
               cv (aget val "changedValue")
               uniqueid (aget val "uniqueid")]
           (aset cv "uniqueid" uniqueid)
           (assoc res parid 
                  (conj exis (js->clj cv))))
         (rest values))))))

(defn map-nodes
  [tree f]
  (let [rez (f tree)]
    (if-let [ch (:children tree)]
      (assoc rez :children (map (fn [n] (map-nodes n f)) ch))
      rez
      )))

                                        ;for easier merging transform the data to be parent data, and then its trivial to bundle everything together
(defn get-data-as-parent
  [data]
;;  (u/debug "data for parent")
;;  (u/debug data)
  (loop [rez [] dta (:data data)]
    (if (empty? dta)
      rez
      (let [d (first dta)
            parid (:parentid d)
            [ind _dd] (first (keep-indexed (fn [i v] (when (= parid (:uniqueid v)) [i v])) rez))
            chd (if _dd 
                  (-> _dd :children first :data (conj (dissoc d :parentid)))
                  [(dissoc d :parentid)])
            rr {:uniqueid parid :children [{:object-name (:object-name data) :data chd}]}
            ]
        (recur (if ind (assoc rez ind rr) (conj rez rr))
               (rest dta))))))


(defn merge-off-data
  [_ln rn]
;;  (u/debug "merge-off-data")
;;  (u/debug _ln)
;;  (u/debug rn)
  (loop [ln (:data _ln) rn rn]
    (if (empty? rn)
      (assoc (dissoc _ln :children) :data ln)
      (let [r (first rn)
            uid (:uniqueid r)
            [ind _dd] (first (keep-indexed (fn [i v] (when (= uid (:uniqueid v)) [i v])) ln))]
        (recur
         (if-not _dd
           (conj ln r)
           (assoc (vec ln) ind 
                  (assoc _dd
                         :children (concat (:children _dd) (:children r)))))
         (rest rn))))))

                                        ;this one will be complete now, the controls.cljs wilj just pass the tree

(declare accumulate-tree)

(defn get-change-tree
  [tree]
  (let [offline-objs (:containers
                      (accumulate-tree tree
                                       (fn [acc n]
                                         (let [_conts (:containers acc)
                                               conts (if _conts _conts [(-> acc :object-name)])]
                                           (assoc acc :containers (conj conts (-> n :object-name)))))))]
    (->
     (p/prom-all (doall (map db/get-changed-object-values offline-objs)))
     (p/then (fn [vals]
             (map-nodes
              tree
              (fn [n]
                (if-let [fltd (first (filter (fn [v] (= (first v) (:object-name n)) ) vals))]
                  (assoc n :data (second fltd))
                  n
                  )))))
     (p/then (fn [data-tree]
;;             (u/debug "data tree")
  ;;           (u/debug data-tree)
             (letfn [(mpt [_tree]
                          (let [_dta _tree
                                _chdta
                                (apply concat
                                       (map (fn [n] (get-data-as-parent (mpt n)))
                                            (:children _tree)))]
                            (if (empty? _chdta)
                              _dta
                              (merge-off-data _dta _chdta))))  
                     ]
               (mpt data-tree)))))))

(defn accumulate-tree
  [tree f]
  (loop [ch (:children tree) rez  tree ]
    (if (empty? ch)
      (f rez tree)
      (recur (rest ch) (f rez  (accumulate-tree (first ch) f))))))


(def test-tree
  {:node {:object-name :a
          :data {nil [{:uniqueid 1 :changed {:a 2 :b 3}}
                      {:uniqueid 2 :changed {:a 4}}]}}
   :children
   [{:node 
     {:object-name :b
      :data {}}} 
    {:node
     {:object-name :c
      :data {1 [{:uniqueid 1 :changed  {:a 5}}]
             2 [{:uniqueid 2  :changed {:b :k}}]
             3 [{:uniqueid 55 :changed {:h :ww}}]}
      }}]
   }
  )

(defn get-parent-node
  [tree node]
  (when (:children tree)
    (let [on (-> node :node :object-name)]
      (if (some (fn [n] (= on (-> n :node :object-name)))
                (:children tree))
        (:node tree)
        (first
         (filter #(-> % nil? not)
                 (map (fn [n]  (get-parent-node n node))
                      (:children tree))))))))

(defn get-grandpa-id
  [tree node parent-id]
  (when-let [pn (get-parent-node tree node)]
    (->
     (dml1 {:type :select-by-key :name (-> pn :node :object-name) :key parent-id :key-name "uniqueid"} true true)
     (p/then (fn [res] (aget res "parentid"))))))

(defn empty-columns-object
  [columns val]
  (loop [ks columns rez {}]
    (if (empty? ks)
      rez
      (recur (rest ks) (assoc rez (first ks) val)))))

;; TODO Urgently test this looks suspicious
;; If the offline inserts happen in the middle of the existing data,
;; update the existing data (increaser townum), and then insert the new row
(defn add-at-index [object-name parent-id rownum]
  (let [uid (rand-int 10000000)
        qbewhere {"rownum" [">=" rownum]}]
    (->
     (get-column-names object-name)
     (p/then (fn [col-names]
               (let [_vals (merge
                            (empty-columns-object col-names "")
                            {"parentid" parent-id
                             "readonly" false
                             "rownum" rownum
                             "uniqueid" (str "new" uid)
                             "changedValue" #js{"_new" true}
                             "new" true
                             "changed" true})
                     _flgs
                     (merge
                      (empty-columns-object col-names (u/transit-json [false false]))
                      {"parentid" parent-id
                       "readonly" false
                       "rownum" rownum
                       "uniqueid" (str "new" uid)})]
                 
                                        ;update of any key in indexeddb is not allowed, I have to delete the records first (parentid,rownum) is the secondary key
                                        ;and then re-insert again
               (->
                (dml [{:type :select :name object-name :qbe qbewhere}
                      {:type :select :name (str object-name "_flags") :qbe qbewhere}])
                (p/then (fn [[selected-data selected-flags]]
                        (loop [data selected-data
                               flags selected-flags rez-data [] rez-flags []]
                          (if (empty? data) 
                            [rez-data rez-flags]
                            (recur (rest data) (rest flags)
                                   (let [_data (first data)
                                         _rnum (inc (:rownum _data))
                                         _statement  (assoc _data :rownum _rnum)]
                                     (conj rez-data _statement))
                                   (let [_flags (first flags)
                                         _rnum (inc (:rownum _flags))
                                         _statement (assoc _flags :rownum _rnum)]
                                     (conj rez-flags _statement)))))))
                (p/then (fn [[old-data old-flags]]
                        (->
                         (dml [{:type :delete :name object-name :qbe qbewhere}
                               {:type :delete :name (str object-name "_flags") :qbe qbewhere}])
                         (p/then (fn []
                                 (dml [{:type :put :name object-name :data old-data}
                                       {:type :put :name (str object-name "_flags") :data old-flags}])
                                 ))
                         (p/then (fn [] 
                                 (dml [{:type :put :name object-name :data _vals}
                                       {:type :put :name (str object-name "_flags") :data _flgs}])))))))))))))



(defn bulk-update-data-and-flags
  [table-name cache-data]
  ;;when alter table has been done, the new columns have been added without the data. The easiest fix is to repopulate data from the cache
  (println "calling bulk update data")
  (loop [dta-u [] flags-u [] cache-data cache-data]
    (if (empty? cache-data)
      (db/update-after-alter table-name {:data dta-u :flags flags-u})
      (let [[rownum {data :data flags :flags}] (first cache-data)]
        (recur
         (if data
           (conj dta-u {:type :update :name table-name :qbe {"rownum" ["=" rownum]} :updateObject (dissoc data "_uniqueid" "_selected")})
           dta-u)
         (if flags
           (conj flags-u {:type :update :name (str table-name "_flags") :qbe {"rownum" ["=" rownum]} :updateObject (dissoc (normalize-data flags) :mboflag)})
           flags-u)
         (rest cache-data))))))

(defn mark-as-preloaded
  [table-name]
  ;;when listToOffline is called, we will mark the flag on metadata. It is assumed that it will not be changed. I will provide the separate methid to clean the meta for all the lists,  if there is need to reload the offline dta
;;  (println "should mark as preloaded table " table-name)
  (updateObjectMeta table-name "preloaded" true)
  (swap! db/preloaded-cache assoc table-name true))

(defn unmark-as-preloaded
  [table-name]
  (updateObjectMeta table-name "preloaded" false)
  (swap! db/preloaded-cache assoc table-name false))



(defn get-lists
  [just-preloaded?]
  ;;get all the preloaded lists
  (->
   (db/select {:qbe {}
               :name "objectMeta"})
   (p/then
    (fn [res]
      (map #(aget % "objectName")
           (filter
            (fn [r]
              (and
               (or (not just-preloaded?) (aget r "preloaded"))
               (.startsWith (aget r "objectName") "list_")))
            res))))))

(defn get-return-column
  [list-name]
  (->
   (db/get-object-meta list-name)
   (p/then
    (fn [res]
      (-> res  (aget "returnColumn") (.toUpperCase))))))

(defn get-qbe-from-select-list
  [list-name]
  (->
   (get-return-column list-name)
   (p/then
    (fn [return-column]
      (->
       (dml1 {:type :select :name list-name :qbe {"_SELECTED" ["=" "Y"]}})
       (p/then
        (fn [res]
          (clojure.string/join
           ","
           (map (fn [r]
                  (str "="
                       (aget r return-column)))
                res)))))))))

(defn remove-selection
  [list-names];;in theory it can be used also for tables
  ;;i will use multiple lists at once to simplify
;;  (println "removing selection for " list-names)
  (dml
   (map (fn [l]
          {:type :update :name l :updateObject {"_SELECTED" nil "changedValue" nil}})
        list-names)))
  
   
