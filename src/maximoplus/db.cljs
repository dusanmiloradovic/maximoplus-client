(ns maximoplus.db
  (:require [maximoplus.idb :as idb]
            [maximoplus.promises :as p]
            [maximoplus.sqlite :as sqlite]
            [maximoplus.utils :as u]
            [clojure.string :as st :refer [join replace split blank? starts-with? includes? trim]])
  (:require-macros
   [maximoplus.db :refer [dml1]])
  (:refer-clojure :exclude [update])
  )

;;This is the abstract dispatcher over the possible offline backends - indexeddb, websql, sqllite. The idea is to create just ddl, dml, ddl1, dml1 functions that return promise and are independent of the underlying engine
;;The offline.cljs will use this, and not the idb.cljs directly

(declare dml)

(declare get-unique-id)

(declare preloaded?)

(def ^:export INDEXEDDB "indexeddb")

(def ^:export WEBSQL "websql")

(def ^:export SQLITE "sqlite")

(def ^:export SQLITENATIVE "sqlitenative")


;;(def ddl-lock (atom (p/get-resolved-promise "start")))

;;the idea is that ddl will block all the operations while dml will not. This should remove the need for the curr-oper promise in the offline. I will do it now and test it just for the sqlite and websql, but it should work fine for indexedDB as well

(defn get-default-engine-name
  []
  ;;if the user
  (if (and
       (exists? js/navigator)
       (= "ReactNative" (aget js/navigator "product")))
    SQLITENATIVE
    (if (exists? js/sqlitePlugin);;Cordova
      SQLITE
      (if (exists? js/openDatabase)
        WEBSQL
        INDEXEDDB))))



(defn column-qbe-true?;if we create the sqllite module for the mobile, this will become unnecessary for that module , and the below function will return the sql where condition
  [data column column-qbe]
  (if (blank? column-qbe)
    true
    (loop [exprs (split column-qbe #",")]
      (if (empty? exprs)
        false
        (if (not= -1 (.indexOf (aget data column) (first exprs)));just the like search for now
          true
          (recur (rest exprs)))))))

(defn get-qbe-operand-and-value
  ;;this should be called on splitted qbe
  ;;supported operands =,!=,>,>=,<,<=
  ;;if no operand assume =
  ;;if there is no operand and qbe contains % or * , the operand is like
  [qbe]
  (when (not= "" (trim qbe))
      (let [operands ["=" "!=" ">" ">=" "<" "<="]]
        (if-let [ex-operand
                 (first
                  (filter
                   (fn [s] (starts-with? qbe s))
                   operands))]
          [ex-operand (trim (subs qbe (count ex-operand)))]
          (if (or
               (includes? qbe "*")
               (includes? qbe "%"))
            ["like" (trim qbe)]
            ["=" (trim qbe)])))))

(defn internal-sqlite-qbe
  ;;from Maximo QBE to internal implementation
  [qbe]
  (into {}
        (filter
         (fn [[k v]] v)
         (map (fn [[k v]]
                [k
                 (when (not= "" (trim v)) 
                   (let [qbvals (map trim (split v ","))
                         mvals (map get-qbe-operand-and-value qbvals)
                         moper (-> mvals first first);;currently only one operand per value is supported
                         ]
                     [moper (map second mvals)]
                     ))])
              (js->clj qbe)))))

(defprotocol DB
  (set-database-name [this db-name])
  (ddl-internal [this objects] [this objects raw?] [this objects raw? readonly?])
  (dml-internal [this objects] [this objects raw?] [this objects raw? readonly?])
  (exist-object-internal? [this object-name])
  (get-object-names-internal [this])
  (-update [this object]);;this is a special case. The problem is that we need sometimes to update the objects that are objects, and there is no obvious way to do it in sqlite, while for indexeddb that is natural update. For sqli
  (-select [this object]);;we have to get the object from JSON store in sqlite
  (-delete [this object]);;delete same as update
  (-get-changed-value [this value])
  (-set-changed-value [this value cval])
  (-convert-changed-value [this cval])
  (-get-offline-qbe-where [this table-name parent-id])
  (-get-unique-id [this rel-name parent-id rownum])
  (-get-flags-array [this val])
  (-get-object-meta [this object-name])
  (-get-column-names [this object-name])
  (-get-changed-object-values [this object-name])
  (-get-return-list-value [this list-table-name rownum return-column])
  (-update-after-alter [this object-name data])
  (-get-internal-qbe [this qbe]);;from Maximo to internal qbe presentation
  )


(deftype IndexedDB[]
  DB
  (set-database-name
    [this db-name]
    (idb/set-database-name db-name)
    )
  (ddl-internal
    [this objects]
    (idb/ddl objects))
  (ddl-internal
    [this objects raw?]
    (idb/ddl objects raw?))
  (ddl-internal
    [this objects raw? readonly?]
    (idb/ddl objects raw? readonly?))
  (dml-internal
    [this objects]
    (idb/ddl objects))
  (dml-internal
    [this objects raw?]
    (idb/ddl objects raw?))
  (dml-internal
    [this objects raw? readonly?]
    (idb/ddl objects raw? readonly?))
  (exist-object-internal?
    [this object-name]
    (->
     (idb/get-database)
     (p/then (fn [db] (u/first-in-arr (aget db "objectStoreNames") #(= % "objectMeta"))))
     ))
  (get-object-names-internal
    [this]
    (->
     (idb/get-database)
     (p/then (fn [db]
               (aget db "objectStoreNames")))
     ))
  (-update
    [this object]
    (idb/dml [(assoc object :type :update)] true));;for idb there is practically no difference
  (-delete
    [this object]
    (idb/dml [(assoc object :type :delete)] true))
  (-select
    [this object]
    (->
     (idb/dml [(assoc object :type :select)] true)
     (p/then (fn [[rez]] rez))
     ))
  (-get-changed-value [this value] (aget value "changedValue"))
  (-set-changed-value [this value cval] (do (aset value "changedValue" cval) value))
  (-convert-changed-value [this cval] cval)
  (-get-offline-qbe-where [this table-name parent-id]
    (->
     (dml1 {:type :select :name "objectQbe" 
            :where (fn [x]
                     (= table-name (:objectName x))
                     )})
     (p/then (fn [[res]]

               (if-let [qbe (:qbe res)]
                 {:where
                  (fn [x]
                    (when (or (not parent-id) (= parent-id (aget x "parentid")))
                      (loop [qbe-cols (keys qbe)]
                        (if (empty? qbe-cols)
                          true
                          (let [qbe-col (first qbe-cols)]
                            (if-not (column-qbe-true? x (name qbe-col) (qbe qbe-col))
                              false
                              (recur (rest qbe-cols))))))))}
                 (if-not parent-id {:where (fn[x] true)} {:where #(= parent-id (aget % "parentid"))}))))))
  (-get-unique-id [this rel-name _parent-id rownum]
    (let [parent-id (if _parent-id _parent-id -1)]
      (->
       (dml
        [{:type :select-by-key  :name rel-name :index-column #js["parentid" "rownum"]  :key #js[parent-id rownum]}] true true)
       (p/then 
        (fn [res]
;;          (u/debug "got the unique id for " rel-name " and rownum " rownum " and parent-id " _parent-id)
          (when-let [rt (-> res (aget 0))]
            (aget rt "uniqueid")))))))
  (-get-flags-array [this val] val)
  (-get-object-meta
    [this object-name]
    (->
     (dml
      [{:type :select-by-key :name "objectMeta" :key object-name :key-name "objectName"}]
      true true)
     (p/then (fn [res]
               (aget res 0)))))
  (-get-column-names
    [this object-name]
    (->
     (dml
      [{:type :select-by-key :name "objectMeta" :key object-name :key-name "objectName"}]
      true true)
     (p/then (fn [_res]
               (let [res (-> _res (aget 0) (aget "columnsMeta"))]
                 (loop [vec [] i 0]
                   (if (= i (.-length res))
                     vec
                     (recur (conj vec (aget (aget res i) "attributeName")) (inc i)))))))))
  (-get-changed-object-values [this object-name]
    (->
     (dml1 {:type :select :name object-name :where :changed})
     (p/then (fn [ok]
               [object-name (map (fn[v] (-> v :changedValue (assoc :uniqueid (:uniqueid v) :parentid (:parentid v)))) ok)]))))
  (-get-return-list-value [this list-table-name rownum return-column]
    (->
     (get-unique-id list-table-name nil rownum)
     (p/then (fn [id]
               (->
                (dml
                 [{:type :select-by-key :name list-table-name :key id :key-name "uniqueid"}] true true)
                (p/then (fn [res]
                          (let [ret-obj (-> res (aget 0))]
                            (aget ret-obj return-column)))))))))
  (-update-after-alter [this object-name data]
    (u/debug "update-after-alter not implemented for indexeddb!"))
  (-get-internal-qbe [this qbe]
    qbe);;TODO once IndexedDB offline is done completely, review
  )

;;WebSql and Sqlite are the same, setting the dialect will handle the difference
(deftype Sqlite[]
  DB
  (set-database-name
    [this db-name]
    (sqlite/set-database-name db-name))
  (ddl-internal
    [this objects]
    (sqlite/ddl objects))
  (ddl-internal
    [this objects raw?]
    (sqlite/ddl objects raw?))
  (ddl-internal
    [this objects raw? readonly?]
    (sqlite/ddl objects raw? readonly?))
  (dml-internal
    [this objects]
    (sqlite/dml objects))
  (dml-internal
    [this objects raw?]
    (sqlite/dml objects raw?))
  (dml-internal
    [this objects raw? readonly?]
    (sqlite/dml objects raw? readonly?))
  (get-object-names-internal
    [this]
    (sqlite/get-object-names))
  (exist-object-internal?
    [this object-name]
    (sqlite/exist-object? object-name))
  (-update
    [this object]
    ;;for sqlite, this will be used just for the non-relational data (i.e. tables with json_store column, and it will update the JSON_STORE column)
    (let [updf (:update object)
          key-name (:key-name object);;we don't have this info for sqlite, must provide it manually
          qbe-where (if  key-name
                      {key-name ["="  (:key object)]}
                      (:qbe object))
          where-f (when-not qbe-where (:where object));;special case, the where will be function on JSON, we have to fetch all the data and filter with this function
          ;;it will be either update by key, and then the :key attribute will be provided, or the :qbe
          ]
      (->
       (sqlite/dml [(assoc object :type :select :qbe qbe-where)])
       (p/then (fn [[rez]]
                 (if-not where-f
                   rez
                   (filter
                    (fn [r]
                      (-> r (aget sqlite/json-store-column) u/read-json where-f))
                    rez))))
       (p/then (fn [filtered]
                 (sqlite/dml
                  (map
                   (fn [r]
                     (let [val (-> r (aget sqlite/json-store-column) u/read-json updf)]
                       (assoc object :type :update :qbe {key-name ["=" (aget r key-name)]} :updateObject {sqlite/json-store-column (u/create-json val)} )))
                   filtered)))))))
  (-select
    [this object]
    (let [key-name (:key-name object);;we don't have this info for sqlite, must provide it manually
          qbe-where (if  key-name
                      {key-name ["=" (:key object)]}
                      (:qbe object))
          where-f (when-not qbe-where (:where object));;special case, the where will be function on JSON, we have to fetch all the data and filter with this function
          ;;it will be either update by key, and then the :key attribute will be provided, or the :qbe
          ]
      (->
       (sqlite/dml [(assoc object :type :select :qbe qbe-where)])
       (p/then (fn [[rez]]
                 (if-not where-f
                   rez
                   (filter
                    (fn [r]
                      (-> r (aget sqlite/json-store-column) u/read-json where-f))
                    rez))))
       (p/then (fn [filtered]
                 (map
                  (fn [r]
                    (-> r (aget sqlite/json-store-column) u/read-json))
                  filtered)
                 )))))
  (-delete
    [this object]
    (->
     (-select this object)
     (p/then (fn [rez]
               (sqlite/dml {:type :delete :name (:name object) :qbe
                            {"uniqueid" ["="
                                         (map
                                          (fn [res]
                                            (-> res (aget sqlite/json-store-column) u/read-json (aget "uniqueid")))
                                          rez)]}}))))
    )
  (-get-changed-value [this value] (when-let [cv  (aget value "changedValue")] (u/read-json cv)))
  (-set-changed-value [this value cval] (do (aset value "changedValue" (u/create-json cval)) value))
  (-convert-changed-value [this cval] (u/create-json cval))
  (-get-offline-qbe-where
    [this table-name parent-id]
    (->
     (-select this {:name "objectQbe" :key table-name :key-name "objectName"})
     (p/then (fn [[res]]
               (let [_qbe (when res (aget res "qbe"))
                     qbe (if-not _qbe {} (internal-sqlite-qbe _qbe))]
                 (if-not parent-id
                   {:qbe qbe}
                   {:qbe (assoc qbe "parentid" ["=" parent-id])}))))))
  (-get-unique-id
    [this rel-name _parent-id rownum]
    (let [parent-id (if _parent-id _parent-id -1)]
      (->
       (dml1 {:type :select :name rel-name :qbe {"parentid" ["=" parent-id] "rownum" ["=" rownum]}})
       (p/then (fn [[res]]
                 (when res
                   (aget res "uniqueid")))))))
  (-get-flags-array
    [this val]
    (if-not val
      [true,false]
      (let [[ro rq] (.split val ",")]
        [(if (= "true" ro) true false) (if (= "true" rq) true false)])))
  (-get-object-meta
    [this object-name]
    (->
     (dml
      [{:type :select-by-key :name "objectMeta" :key object-name :key-name "objectName"}]
      true true)
     (p/then (fn [res]
               ;;      (println "get-object-meta" res ".." (-> res first) "..." (-> res first first))
               (when-not (empty? res)
                 (when-let [obj (-> res first first)]
                   (-> obj (aget sqlite/json-store-column) u/read-json )))))))
  (-get-column-names
    [this object-name]
    (->
     (-get-object-meta this object-name)
     (p/then (fn [_res]
               (let [res (-> _res (aget "columnsMeta"))]
                 (loop [vec [] i 0]
                   (if (= i (.-length res))
                     vec
                     (recur (conj vec (aget (aget res i) "attributeName")) (inc i)))))))))
  (-get-changed-object-values
    [this object-name]
    (->
     (dml1 {:type :select :name object-name :sqlwhere " changedValue is not null"})
     (p/then (fn [ok]
               [object-name (map
                             (fn [v] (-> v
                                         (aget "changedValue")
                                         u/read-json
                                         (js->clj :keywordize-keys true)
                                         (assoc :uniqueid (aget v "uniqueid") :parentid (aget v "parentind"))))
                             ok)]))))
  (-get-return-list-value
    [this list-table-name rownum return-column]
    (->
     (get-unique-id list-table-name nil rownum)
     (p/then
      (fn [id]
        (->
         (dml1 {:name list-table-name :type :select-by-key :key id :key-name "uniqueid"})
         (p/then (fn [_res]
                   (aget (first _res) return-column))))))))
  (-update-after-alter [this object-name data]
    (sqlite/update-after-alter object-name data))
  (-get-internal-qbe [this qbe]
    (let [_qbe (js->clj qbe)]
      (map (fn [[k v]]
             [k v])
           _qbe)))
  )

(def engine  (atom nil))

(defn ^:export setEngine
  [engine-type]
  (reset! engine
          (do
;;            (u/debug engine-type)
            (case engine-type
              "indexeddb" (IndexedDB.)
              "websql" (do
                         (reset! sqlite/dialect "WEBSQL")
                         (Sqlite.))
              "sqlite" (do
                         (reset! sqlite/dialect "SQLITE")
                         (Sqlite.))
              "sqlitenative" (do
                               (reset! sqlite/dialect "SQLITENATIVE")
                               (Sqlite.))
              )))
  )

(setEngine (get-default-engine-name))



(defn ^:export setDatabaseName
  [dbName]
  (set-database-name @engine dbName))

(defn ^:export ddl
  ([objects]
   (ddl-internal @engine objects))
  ([objects raw?]
   (ddl-internal @engine objects raw?))
  ([objects raw? readonly?]
   (ddl-internal @engine objects raw? readonly?))
  )

(defn filter-preloaded
  [objects]
  ;;  (println "filter-preoloaded " objects)
  (let [vec-prom (map (fn [object]
                        (let [operation-type (:type object)
                              object-name (:name object)]
                          (->
                           (preloaded? (:name object))
                           (p/then
                            (fn [prel?]
                  ;;            (println "preloaded?" (:name object) "=" prel?)
                              (when (or (= :select operation-type )
                                        (= :select-by-key operation-type )
                                        (= :select-all operation-type )
                                        (not prel?))
                                object))))))
                      objects)]
    ;;    (println "vec-prom " vec-prom)
    (->
     (p/prom-all-new
      vec-prom
      )
     (p/then
      (fn [rez]
        (remove not rez))))))

(defn ^:export dml
  ([objects]
   (->
    (filter-preloaded objects)
    (p/then (fn [_objects]
    ;;          (println "dml " _objects)
              (dml-internal @engine _objects)))))
  ([objects raw?]
   (->
    (filter-preloaded objects)
    (p/then (fn [_objects]
  ;;            (println "dml " _objects)
              (dml-internal @engine _objects raw?))))
   )
  ([objects raw? readonly?]
   (->
    (filter-preloaded objects)
    (p/then (fn [_objects]
;;              (println "dml " _objects)
              (dml-internal @engine _objects raw? readonly?))))))

(defn ^:export exist-object?
  [object-name]
  (exist-object-internal? @engine object-name))

(defn ^:export get-object-names
  []
  (get-object-names-internal @engine))

(defn ^:export getDefaultEngine
  []
  
  )



(defn ^:export select [object]
  (-select @engine object))

(defn ^:export update [object]
  (-update @engine object))

(defn ^:export delete [object]
  (-delete @engine object))

(defn ^:export get-changed-value [value]
  (-get-changed-value @engine value))

(defn ^:export set-changed-value [value cval]
  (-set-changed-value @engine value cval))

(defn ^:export convert-changed-value [cval]
  (-convert-changed-value @engine cval))

(defn get-offline-qbe-where [table-name parent-id]
  (-get-offline-qbe-where @engine table-name parent-id))

(defn get-unique-id [rel-name parent-id rownum]
  (-get-unique-id @engine rel-name parent-id rownum)
  )

(defn get-flags-array [val]
  (-get-flags-array @engine val))

(defn get-object-meta [object-name]
  (-get-object-meta @engine object-name))

(defn get-column-names [object-name]
  (-get-column-names @engine object-name))

(defn get-changed-object-values
  [object-name]
  (-get-changed-object-values @engine object-name))

(defn get-return-list-value
  [list-table-name rownum return-column]
  (-get-return-list-value @engine list-table-name rownum return-column))

(defn update-after-alter
  [table-name data]
  (-update-after-alter @engine table-name data)
  )

(def preloaded-cache (atom {}))

(defn preloaded?
  [table-name]
;;  (println "PRELOAD CACHE:" @preloaded-cache)
  (if (= "objectMeta" table-name)
    (p/get-resolved-promise false)
    (let [cached (@preloaded-cache table-name)]
      ;;can be list or the preloaded container
      (if (nil? cached)
        (if table-name;;maybe list table has not been created yet
          (->
           (get-object-meta table-name)
           (p/then
            (fn [meta]
              (when meta
  ;;              (println "--------------->" meta)
                (aget meta "preloaded"))))
           (p/then
            (fn [preloaded?]
              (swap! preloaded-cache assoc table-name preloaded?)
              preloaded?
              )))
          (p/get-resolved-promise false))
        (p/get-resolved-promise cached)))))


