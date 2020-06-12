(ns maximoplus.sqlite
  (:require [clojure.set :refer [difference]]
            [clojure.string :refer [replace split join]]
            [maximoplus.utils :as u]
            [maximoplus.promises :as p]
            [maximoplus.arrays :as ar]
            )
  
  )

(def databaseName (atom "maxPlus"))

(def database (atom nil))

;;in React Native, we use Expo SQLite db, and that has to be created in Expo App first, and then set here
(defn ^:export setDatabase
  [db]
  (reset! database db))

(def dialect (atom nil));;dialect set from db.cljs

(def json-store-column "JSON_STORE");MAYBE a better name is requiredc

;;the only difference I see is the open database

(defn  set-database-name [db-name]
  (reset! databaseName db-name)
  )

(def columns-lock (atom {}))

(declare get-table-columns)

(defn get-new-columns-lock
  [table-name]
  ;; For create and alter statements, always get a new one unless the old one hs not been released yet
  (let [cl (@columns-lock table-name)]
    (if (or (not cl) (p/has-fired? cl))
      (let [d (p/get-deferred)]
        (swap! columns-lock assoc table-name d)
        d)
      cl)))

(defn get-columns-lock
  [table-name]
  (if-let [cl (@columns-lock table-name)]
    cl
    (let [nl (get-new-columns-lock table-name)]
      (->
       (get-table-columns table-name)
       (p/then
        (fn [cols]
          (when cols
            (p/callback nl cols)))))
      nl
      )))

;;Instead of the create-lock which is difficult to synchronize, I will create the columns-lock. If the table doens't exist, once it is created the promise will be populated with the list of columns. If it does, it will query the sqlite, find the columns and populate the promise.
;;One more important case is altering the table. In that case, the new promise has to be created

;;prepareDatabase: if we want to prepare the database that was created (for example with the DB script)
;;for the native applications, we could load the database from the file, and for that we should use the
;;getSQLDatbase (it loads the complete database, instead of creating the file and running the script)

(def ^:export globalFunctions
  #js{"getSQLDatabase" (fn[database-name]
                         (.openDatabase js/window database-name "1.0" database-name (* 5 1024 1024)))
      "prepareDatabase" (fn [] (p/get-resolved-promise true))})

(defn get-database
  []
  (when-not @database
    (reset! database
            (->
             (p/get-resolved-promise
              (.call (aget globalFunctions "getSQLDatabase") nil @databaseName))
             (p/then
              (fn [db]
                (-> (.call (aget globalFunctions "prepareDatabase") nil)
                    (p/then
                     (fn [] db))))))))
  @database)

(defmulti sql-oper (fn [operation] (:type operation)))


(defn get-object-names
  []
  (->
   (get-database)
   (p/then
    (fn [db]
      (p/get-promise
       (fn [resolve reject]
         (.transaction
          db
          (fn [tx]
            (.executeSql
             tx
             "select name from sqlite_master where type='table'"
             #js[]
             (fn [tx results]
               (let [rows (aget results "rows")
                     _len (aget rows "length")
                     rez #js[]]
                 (dotimes [i _len]
                   (ar/conj! rez (.item rows i)))
                 (resolve rez))))))))))))

(defn exist-object?
  [object-name]
  (->
   (get-object-names)
   (p/then (fn [names]
             (some (fn [x] (= (aget x "name") object-name)) names)))))

(def columns-cache (atom {}))

(defn get-table-columns
  [table-name]
  (->
   (get-database)
   (p/then
    (fn [db]
      (p/get-promise
       (fn [resolve reject]
         (.transaction
          db
          (fn [tx]
            (.executeSql
             tx
             "SELECT name, sql FROM sqlite_master WHERE type='table' AND name = ?"
             #js[table-name]
             (fn[tx results]
               (let [rows (-> results (aget "rows"))]
                 (if (> (aget rows "length") 0)
                   (let [row (.item rows 0)
                         _sql (aget row "sql")
                         col-string (replace _sql #"^.+\((.+)\).*" "$1")
                         splitted (split col-string #",\s?")
                         columns  (map
                                   (fn[s] (-> s (clojure.string/split " ") first))
                                   splitted)]
                     (swap! columns-cache assoc table-name columns)
                     (resolve columns))
                   (resolve nil))))
             (fn [tx err]
               (.log js/console err)
               ;;dont reject, probably no table is there
               (resolve nil)))))))))))

(defn get-data-type
  [column-meta]
  (if (or (= "DATE" (get column-meta "maxType"))
          (= "DATETIME" (get column-meta "maxType")))
    "integer" "text"))

(defn get-columns-string
  [key key-type index-columns columns-meta]
  ;;if the columns-meta is not there this is the two columns table with uniqueid and json-store used for storing the objects. If it is there, we need to add the columns that will be used by the framework (uniqueid, parentid...)
  (let [key-str
        (if key
          (str key " " key-type " primary key ")
          "uniqueid integer primary key autoincrement")]
    (str key-str
         (if columns-meta
           (str
            (apply str
                   (map (fn [v]
                          (let [attr-name (:attributeName v)
                                data-type (get-data-type v)]
                            (if attr-name (str ",\"" attr-name "\" " data-type) "")))
                      columns-meta))
            ", parentid integer default 0, rownum integer default 0, readonly integer default 0, new integer default 0, changed integer default 0, changedValue text, tabsess text"
            )
           (str ", " json-store-column " text")
           ))))

(defn get-alter-table-string
  [object-name existing-columns columns-meta]
  (let [fm (fn [x] (map (comp #(clojure.string.replace % "\"" "") clojure.string.upper-case) x))]
    (when-let [new-cols 
               (difference
                (set
                 (fm (filter #(-> % nil? not) (map :attributeName columns-meta))))
                (set (fm existing-columns)))]
      (map (fn [c]
             (let [m
                   (first
                    (filter
                     #(= (get % :attributeName) c) columns-meta))]
               (str "alter table " object-name " add \"" c "\" " (get-data-type m))))
           new-cols))))

;;object_store column will store the json for the unstructured data. For the time being it can be either or - if there is unstructured data (used for example for storing the offline workflow, metadata ...) the columns will be ingored, if there are columns unstructered data will be ignored



(def create-table-lock (atom {}))

(def  waiting-for-alter (atom {}))

(declare dml)

(defn prepare-statements
  [statements cols]

  (let [_cols (map
               (fn [s]
                 (clojure.string/replace s "\"" "" ))
               cols)]
    (map (fn [s]
           (update-in s [:updateObject]
                      (fn [o]
                        (select-keys o _cols))))
         statements)))

(defmethod sql-oper :create [k]
  (let [object-name (:name k)
        key (:key k)
        key-type (if-let [k (:keyType k)] k "integer")
        index-columns (:index-columns k)
        columns-meta (:columns-meta k)
        d (get-new-columns-lock object-name)
        create-lock (if-let [tl (@create-table-lock object-name)]
                      tl
                      (p/get-resolved-promise "ok")
                      )]
    (swap!
     create-table-lock
     assoc
     object-name
     (->
      create-lock
      (p/then
       (fn []
         (get-database)))
      (p/then
       (fn [db]
         (->
          (get-table-columns object-name)
          (p/then
           (fn [columns]
             ;;        (u/debug ":create for object " object-name)
             (u/debug ":create for " object-name " got columns")
             ;;       (.log js/console (clj->js columns))
             (if-not columns ;;new table
               (let [create-string (str "create table " object-name " (" (get-columns-string key key-type index-columns columns-meta ) ")")]
                 ;;             (.log js/console create-string)
                 (p/get-promise
                  (fn [resolve reject]
                    (.transaction
                     db
                     (fn [tx]
                       (.executeSql
                        tx
                        create-string
                        #js[]
                        (fn [tx res]
                          (p/then (get-table-columns object-name)
                                  (fn [cols]
                                    ;;                                (u/debug ":create " object-name " finished")
                                    (p/callback d cols)
                                    (resolve cols))))
                        (fn [tx err]
                          (p/callback d err)
                          (reject err))))))))
               (when-let [alter-table-string (get-alter-table-string object-name columns columns-meta)]
                 (p/get-promise
                  (fn [resolve reject]
                    (.transaction
                     db
                     (fn [tx]
                       (doseq [s alter-table-string]
                         (.executeSql tx s #js[])))
                     (fn [err]
                       (p/callback d err)
                       (reject err))
                     (fn []
                       (p/then (get-table-columns object-name)
                               (fn [cols]
                                 ;;                             (u/debug ":alter table " object-name " finished")
                                 (when-not (empty? (@waiting-for-alter object-name))
                                   (let [data (prepare-statements(:data  (@waiting-for-alter object-name)) cols)
                                         flags (prepare-statements (:flags  (@waiting-for-alter object-name)) cols)]

                                     (dml (concat data flags))))
                                 (p/callback d cols)
                                 (resolve cols))))))))))))))))))

(def ^:dynamic BULK_INSERT_LIMIT 500) ;;limit set by SQlite itself

(defn get-insert-data-seq
  [columns data]
  (map (fn [c] (if (= c json-store-column)
                 (u/create-json (clj->js data))
                 (get data c)))
       columns))

(defn get-insert-into
  [object-name]
  (->
   (get-columns-lock object-name)
   (p/then (fn [cols]
             ;;we can't use the promise directly, because we have the promise is fired when the table is created, but it can be altered in the meantime
             [cols
              (str "insert or replace into " object-name "("
                   (join "," cols ) ")")
              (str "("
                   (join "," (map (fn [_] "?")  cols)) ")")]
             ))))

(defn remove-quotes-for-columns
  [cols]
  (map (fn [c] (replace c #"\""  ""))
       cols))

(defn get-put-statement
  [data object-name]
  [(->
    (get-insert-into object-name)
    (p/then (fn [[cols insert-s values-s]]
              [(str insert-s " values " values-s)
               (clj->js (get-insert-data-seq (remove-quotes-for-columns cols) data))]
              )))])

(defn get-multiple-put-statements
  [data object-store]
  (loop [dta data rez []]
    (if (empty? dta)
      rez
      (let [curr (take BULK_INSERT_LIMIT dta)] 
        (recur (drop  BULK_INSERT_LIMIT dta)
               (conj rez
                     (->
                      (get-insert-into object-store)
                      (p/then (fn [[cols ins-s values-s]]
                              (let [_cols (remove-quotes-for-columns cols)]
                                [(str ins-s " values " (join "," (repeat (count curr) values-s)))
                                 (clj->js (mapcat (fn [d] (get-insert-data-seq _cols d)) curr))]))))))))))

(defprotocol ObjectStore
  (put [data object-store]))

(extend-protocol ObjectStore
  PersistentHashMap
  (put [data object-store]
    (get-put-statement data object-store)
  )
  PersistentArrayMap
  (put [data object-store]
    (get-put-statement data object-store))
  object
  (put [data object-store]
    (get-put-statement (js->clj data) object-store))
  PersistentVector
  (put [data object-store]
    (get-multiple-put-statements data object-store))
  array
  (put [data object-store]
    (get-multiple-put-statements (js->clj data) object-store))
  LazySeq
  (put [data object-store]
    (get-multiple-put-statements data object-store))
  )

(defmethod sql-oper :put [k]
  (let [object-name (:name k)
        data (:data k)]
    (put data object-name)))



;;qbe syntax: {"b" ["=" 2] "c" ["like" ["'a%'" 3]]}
;;column [operator val|list-of val]
;;if we have the list-of val after operator that means OR
;;TODO (low priority) multiple operators per column (i.e ='a' or !='b' or !='c' for example(
(defn get-qbe-where
  [qbe]
  (if-not (empty? qbe)
    (let [bf (fn [k op v]
               (if (sequential? v)
                 (let [[s b]
                       (reduce
                        (fn [[s b] [vs vb]]
                          (if-not s
                            [vs (conj b vb)]
                            [(str s " or " vs) (conj b vb)]
                            ))
                        [nil []]
                        (map (fn [vv]
                               [(str k " " op " ?") vv])
                             v))
                       ]
                   [(str "(" s ")") b])
                 [(str k " " op " ?" ) [v]]
                 ))]
      (reduce
       (fn [[s1 b1] [s2 b2]]
         [(str s1 " and " s2) (concat b1 b2)])
       (map (fn [[k [op v]]]
              (bf k op v))
            qbe)))
    ["1=1" []];;qbe where qbe binds
    ))

;;just for the update, update object is specified
;;syntax {"b" "bla" "c "cla"}

(defn get-update-statement
  [update-attrs]
  (reduce
   (fn [[s b] [vs vb]]
     (if-not s
       [vs (conj b vb)]
       [(str s "," vs) (conj b vb)]))
   [nil []]
   (map
    (fn [[k v]]
      [(str "\"" k "\"=?") v])
    update-attrs)))

(defmethod sql-oper :select [k]
  (let [object-name (:name k)
        start-row (:start k)
        numrows (:rows k)
        sql-where (:sqlwhere k);;in the future, maybe give the option to the user
        where (:where k);;javascript where
        qbe (:qbe k) ;;qbe will be used for sql where
        [qbe-where qbe-binds] (get-qbe-where qbe)
        order-by (:order-by k)
        ]
    (when-not object-name (throw (js/Error. "No object name")))
    (when (and qbe sql-where) (throw (js/Error. "Both qbe and sqlwhere present")))
    [[(str "select rowid, * from " object-name
           (when sql-where (str " where " sql-where))
           (when qbe-where (str (if sql-where " and " " where ") qbe-where))
           (when order-by (str " order by " order-by))
           (when numrows (str " LIMIT " numrows (when start-row (str " OFFSET " start-row)))))
      (if qbe-binds (clj->js qbe-binds)  #js[]) ]]
    ))

(defmethod sql-oper :delete [k]
  (let [object-name (:name k)
        [qbe-where qbe-binds] (get-qbe-where (:qbe k))
        ]
;;   (println "##deleting for " object-name " and " qbe-where " and " qbe-binds)
    [[(str "delete from " object-name (when qbe-where (str " where " qbe-where)))
      (if qbe-binds (clj->js qbe-binds)  #js[])]])
  )



(defmethod sql-oper :update [k]
  (let [object-name (:name k)
        update-f (:update k)
        [qbe-where qbe-binds] (get-qbe-where (:qbe k))
        [update-set update-binds] (get-update-statement (:updateObject k))]
    [[(str "update " object-name " set " update-set (when qbe-where (str " where " qbe-where)) ) (clj->js (concat update-binds qbe-binds))]]
    ))

;;there is no reason to complicate thigs, I keep the function with the same name becuase of the indexDB compatibility, it will be identical here with :update
(defmethod sql-oper :update-by-key [k]
  (sql-oper (assoc k :type :update :qbe {(:key-name k) ["="  (:key k)]}))
  )

;;same goes to the select-by-key
(defmethod sql-oper :select-by-key [k]
;;  (.log js/console (clj->js k))
;;  (u/debug "SQL:" (assoc k :type :select :qbe {(:key-name k) ["="  (:key k)]}))
  (sql-oper (assoc k :type :select :qbe {(:key-name k) ["="  (:key k)]})))

(defmethod sql-oper :select-all [k]
  (sql-oper (assoc k :type :select)));;the where clause will be omitted

(defn dml
  [operations & ops]
  (let [raw? (first ops)]
    (->
     (get-database)
     (p/then
      (fn [db]
        (p/then
         (p/prom-all
          (mapcat sql-oper operations))
         (fn [arr]
           (p/get-promise
            (fn [resolve reject]
              (.transaction
               db
               (fn [tx]
                 (p/then
                  (p/prom-all
                   (loop [arr arr rez []]
                     (if (empty? arr)
                       rez
                       (let [a (first arr)
                             prm (p/get-promise
                                  (fn [_resolve _reject]
                                    ;;                                (u/debug "^^^^^" (first a) ".." (second a))
                                    (.executeSql tx (first a) (second a)
                                                 (fn [tx ok] (_resolve ok)))))]
                         (recur (rest arr) (conj rez prm))))))
                  (fn [rezarr]
                    (let [rez-js
                          (ar/mape
                           (fn [r]
                             (let [rows (aget r "rows")
                                   rlen (aget rows "length")
                                   rez #js[]]
                               (dotimes [i rlen]
                                 (ar/conj! rez (.item rows i) ))
                               rez))
                           rezarr)
                          ]
                      (resolve
                       (if raw?
                         rez-js
                         (clj->js rez-js))
                       )))))
               (fn [err]
                 (println "!!!!!!!!!!" operations)
                 (reject err)))))))))))) 

(defn ddl
  [operations & ops]
  (p/prom-all
   (map sql-oper operations))
  )


(defn test-sqlite-create
  []
  (ddl
   [{:type :create :name "radnici" :key "id"}
    {:type :create :name "stolovi" :key "stoid"
     :columns-meta [{:attributeName "b"} {:attributeName "c"} {:attributeName "d"}]
     }]))



(defn update-after-alter
  [object-name data]
  (swap! waiting-for-alter assoc object-name data))
