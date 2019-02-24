(ns maximoplus.sqlite
  (:require [clojure.set :refer [difference]]
            [clojure.string :refer [replace split join]]
            [maximoplus.utils :as u]
            [maximoplus.promises :as p]
            [maximoplus.arrays :as ar])
  
  )

(def databaseName (atom "maxPlus"))

(def database (atom nil))

(def dialect (atom "WEBSQL"))

(def json-store-column "JSON_STORE");MAYBE a better name is required

;;the only difference I see is the open database

(defn  set-database-name [db-name]
  (reset! databaseName db-name)
  )

(def create-lock (atom {}))

(defn get-create-lock
  [table-name]
  (if-let [lck (@create-lock table-name)]
    lck
    (let [d (p/get-deferred)]
      (swap! create-lock assoc table-name d)
      d)))

(defn get-database
  []
  (when-not @database
    (reset! database
            (case @dialect
              "WEBSQL" (.openDatabase js/window @databaseName "1.0" @databaseName (* 5 1024 1024))
              "SQLITE" (.openDatabase (aget js/window "sqlitePlugin") #js{"location" "default" "name" @databaseName})
              "SQLITENATIVE" (.openDatabase js/SQLite @databaseName "1.0" @databaseName (* 100 1024 1024)
                                              (fn [] (.log js/console "Sqlite datase open"))
                                              (fn [err] (.log js/console (str "Sqlite database failed" err)))))))
  @database)

(defmulti sql-oper (fn [operation] (:type operation)))


(def table-columns-cache (atom {}))

(defn get-object-names
  []
  (p/get-promise
   (fn [resolve reject]
     (.transaction
      (get-database)
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
             (resolve rez)))))))))

(defn exist-object?
  [object-name]
  (->
   (get-object-names)
   (p/then (fn [names]
           (some (fn [x] (= (aget x "name") object-name)) names)))))

(defn get-table-columns
  [table-name]
  (p/get-promise
   (fn [resolve reject]
     (.transaction
      (get-database)
      (fn [tx]
        (.executeSql tx
                     "SELECT name, sql FROM sqlite_master WHERE type='table' AND name = ?"
                     #js[table-name]
                     (fn[tx results]
                       (let [rows (-> results (aget "rows"))]
                         (if (> (alength rows) 0)
                           (let [row (aget rows 0)
                                 _sql (aget row "sql")
                                 col-string (replace _sql #"^.+\((.+)\).*" "$1")
                                 splitted (split col-string #",\s?")]
                             (resolve
                              (map
                               (fn[s] (-> s (clojure.string/split " ") first))
                               splitted)))
                           (resolve nil))))
                     (fn [tx err]
                       (.log js/console err)
                       ;;dont reject, probably no table is there
                       (resolve nil))))))))

(defn get-table-columns-from-cache
  [table-name]
  (if-let [tcols (@table-columns-cache table-name)]
    (p/get-resolved-promise tcols)
    (-> (get-create-lock table-name)
        (p/then
         (fn [_]
           (get-table-columns table-name))))))

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
            ", parentid integer default 0, rownum integer default 0, readonly integer default 0, new integer default 0, changed integer default 0, changedValue text"
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



(defmethod sql-oper :create [k]
  (let [object-name (:name k)
        key (:key k)
        key-type (if-let [k (:keyType k)] k "integer")
        index-columns (:index-columns k)
        columns-meta (:columns-meta k)
        d (get-create-lock object-name)
        is-deferred? (not (p/has-fired? d))]
    (u/debug ":create " object-name " starting")
    (swap! create-lock assoc
           object-name
           (->
            (if is-deferred? (p/get-resolved-promise "startit") d) ;;it may be already set by another function, and it is waiting on the deferred. We have to resolve that deferred once the table was created, but not block the creation itself
            (p/then (fn [_]
                      (get-table-columns object-name)))
            (p/then
             (fn [columns]
               ;;        (u/debug ":create for object " object-name)
               (u/debug ":create for " object-name " got columns")
               (.log js/console (clj->js columns))
               (if-not columns ;;new table
                 (let [create-string (str "create table " object-name " (" (get-columns-string key key-type index-columns columns-meta ) ")")]
                   (.log js/console create-string)
                   (p/get-promise
                    (fn [resolve reject]
                      (.transaction
                       (get-database)
                       (fn [tx]
                         (.executeSql
                          tx
                          create-string
                          #js[]
                          (fn [tx res]
                            (p/then (get-table-columns object-name)
                                    (fn [cols]
                                      (u/debug ":create " object-name " finished")
                                      (swap! table-columns-cache assoc object-name cols)
                                      (when is-deferred? (p/callback d cols))
                                      (resolve cols))))
                          (fn [tx err]
                            (when is-deferred? (p/callback d err))
                            (reject err))))))))
                 (when-let [alter-table-string (get-alter-table-string object-name columns columns-meta)]
                   (p/get-promise
                    (fn [resolve reject]
                      (.transaction
                       (get-database)
                       (fn [tx]
                         (doseq [s alter-table-string]
                           (.executeSql tx s #js[])))
                       (fn [err]
                         (when is-deferred? (p/callback d err))
                         (reject err))
                       (fn []
                         (p/then (get-table-columns object-name)
                                 (fn [cols]
                                   (swap! table-columns-cache assoc object-name cols)
                                   (when is-deferred? (p/callback d cols))
                                   (resolve cols)))))))))))))))

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
   (get-table-columns-from-cache object-name)
   (p/then (fn [cols]
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
    (get-create-lock object-name)
    (p/then (fn []
              (get-insert-into object-name)))
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
;;    (when-not (@table-columns-cache object-name)
    ;;      (swap! table-columns-cache assoc object-name (get-table-columns object-name)))
    ;; not clear why we have it here, leave it as a reminder, and remove after the testing
    
    (put data object-name)))



;;qbe syntax: {"b" ["=" 2] "c" ["like" ["'a%'" 3]]}
;;column [operator val|list-of val]
;;if we have the list-of val after operator that means OR

(defn get-qbe-where
  [qbe]
  (when qbe
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
            qbe)))))

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
      [(str k "= ?") v])
    update-attrs)))

(defmethod sql-oper :select [k]
  (println k)
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
           (when qbe-where (str " where " qbe-where))
           (when order-by (str " order by " order-by))
           (when numrows (str " LIMIT " numrows (when start-row (str " OFFSET " start-row)))))
      (if qbe-binds (clj->js qbe-binds)  #js[]) ]]
    ))

(defmethod sql-oper :delete [k]
  (let [object-name (:name k)
        [qbe-where qbe-binds] (get-qbe-where (:qbe k))
        ]
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
  (u/debug "SQL:" (assoc k :type :select :qbe {(:key-name k) ["="  (:key k)]}))
  (sql-oper (assoc k :type :select :qbe {(:key-name k) ["="  (:key k)]})))

(defmethod sql-oper :select-all [k]
  (sql-oper (assoc k :type :select)));;the where clause will be omitted

(defn dml
  [operations & ops]
  (let [raw? (first ops)]
    (p/then
     (p/prom-all
      (mapcat sql-oper operations))
     (fn [arr]
       (p/get-promise
        (fn [resolve reject]
          (.transaction
           (get-database)
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
           (fn [err] (reject err))))))))) 

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
