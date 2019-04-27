(ns maximoplus.idb
  (:require
   [clojure.set  :refer [difference]]
   [maximoplus.promises :as p]
   )
  )

(def databaseName (atom "maxPlus"))

(def database (atom nil))

(defn ^:export set-database-name [db-name]
  (reset! databaseName db-name)
)





(def curr-idb-oper (atom (p/get-resolved-promise "start")))

(declare exist-object-stores?)


(defn set-curr-idb-oper! 
  [promise description]
  (swap! curr-idb-oper (fn [_] promise))
  )

(defn get-database
  []
  (.log js/console "get-database")
  (if @database (p/get-resolved-promise @database)
      (p/get-promise (fn [resolve reject]
                  (let [req (.open js/indexedDB @databaseName)]
                    (set! (.-onsuccess req) 
                          (fn [evt]
                            (.log js/console "get database on success")
                            (let [db (-> evt (aget "target") (aget "result"))]
                              (set! (.-onversionchange db)
                                    (fn[evt]
                                      (.log js/console "########### closing the DB onversionchange get-database")
                                      (swap! database (fn [_] nil))
                                      (.. evt -target (close))))
                              (swap! database (fn [_] db))
                              (resolve db))))
                    (set! (.-onerror req) 
                          (fn [evt]
                            (.log js/console "get database on error")
                            (reject (-> evt (aget "target") (aget "errorCode")))))
                    (set! (.-onblocked req)
                          (fn [blocked]
                            (.log js/console "get database blocked")
                            (reject blocked))))))))

(defn get-objectstore-names
  []
  (->
   (get-database)
   (p/then
    (fn [db]  (.-objectStoreNames db)))))

(defprotocol ObjectStore
  (add [data object-store])
  (put [data object-store])
  (get-index-name [index-column object-name])
  )

(extend-protocol ObjectStore
  PersistentHashMap
  (add [data object-store]
    (.add object-store (clj->js data)))
  (put [data object-store]
    (.put object-store (clj->js data)))
  PersistentArrayMap
  (add [data object-store]
    (.add object-store (clj->js data)))
  (put [data object-store]
    (.put object-store (clj->js data)))
  object
  (add [data object-store]
    (.add object-store data))
  (put [data object-store]
    (.put object-store  data))
  PersistentVector
  (add [data object-store]
    (doseq [d data]
      (add d object-store)))
  (put [data object-store]
    (doseq [d data]
      (put d object-store)))
  (get-index-name [index-column object-name]
    (get-index-name (clj->js index-column) object-name))
  array
  (add [data object-store]
    (doseq [d data]
      (add d object-store )))
  (put [data object-store]
    (doseq [d data]
      (put d object-store)))
  (get-index-name [index-column object-name]
    (str object-name "_" (.join index-column "_"))
    )
  string
  (get-index-name [index-column object-name]
    (str object-name "_" index-column))
  LazySeq
  (add [data object-store]
    (doseq [d data]
      (add d object-store)))
  (put [data object-store]
    (doseq [d data]
      (put d object-store)))
  )



(defn exist-object-stores?
  [object-stores]
  (->
   (get-objectstore-names)
   (p/then (fn [existing]
           (.log js/console (str "exist-object-stores? " object-stores " and " existing))
           (empty?
            (difference
             (set object-stores)
             (set (array-seq existing))))))))

(defmulti idb-oper (fn [operation] (:type operation)))

(defmethod idb-oper :create [k]
  (let [database (:database k)
        object-name (:name k)
        key (:key k)
        index-columns (:index-columns k)]
    (.log js/console (str "creating object store" object-name " key path " (clj->js key)))
                                        ;assume it is running in onpugradeneeded method
                                        ;there is no point running the create in promise, because the transaction event is fired just once, when the transaction for all the objects have been finished
    (let [object-store
          (if key
            (.createObjectStore database object-name #js {:keyPath (clj->js key)})
            (.createObjectStore database object-name #js {:autoIncrement true}))

          ]
      (doseq [ic index-columns]
        (.createIndex object-store (get-index-name ic object-name) ic)
        )
      (.. object-store -transaction))))

(defmethod idb-oper :insert [k]
  (let [object-store (:object-store k)
        data (:data k)]
    (.log js/console (str "inserting the data into object store" (:name k)))
    (add data object-store)
    (p/get-resolved-promise data)
                                        ;for insert just resolve promise with the data being inserted. Error and completion handling will be done in the main dml transaction
    ))

(defmethod idb-oper :put [k]
  (let [object-store (:object-store k)
        data (:data k)]
    (.log js/console (str "inserting the data into object store" (:name k)))
    (put data object-store)
    (p/get-resolved-promise data)
                                        ;for insert just resolve promise with the data being inserted. Error and completion handling will be done in the main dml transaction
    ))

(defmethod idb-oper :select-by-key [k]
  (let [object-store (:object-store k)
        key (:key k)
        index-column (:index-column k)
        index-name (when index-column (get-index-name index-column (:name k)))
        req (if index-name
              (.get  (.index object-store index-name) key)
              (.get object-store key))
        update (:update k)
        raw? (:raw k)
        ]
    (p/get-promise (fn [resolve reject]
                (set! (.-onerror req)
                      (fn [err] (reject err)))
                (set! (.-onsuccess req)
                      (fn [evt]
                        (let [_res_ (.. evt -target -result)
                              _res (if raw? _res_ (js->clj _res_))
                              res (if update (update _res) _res)]
                          (if update
                            (let [update-req (.put object-store res)]
                              (set! (.-onerror update-req)
                                    (fn [err] (reject err)))
                              (set! (.-onsuccess update-req)
                                    (fn [ok] (resolve res))))
                            (resolve res)
                            ))))))))

(defmethod idb-oper :update-by-key [k]
  (idb-oper (assoc k :type :select-by-key));just a synonym
  )

(defmethod idb-oper :delete-by-key [k]
  (let [object-store (:object-store k)
        key (:key k)
        req (.delete object-store key)]
    (p/get-promise (fn [resolve reject]
                (set! (.-onerror req)
                      (fn [err] (reject err)))
                (set! (.-onsuccess req)
                      (fn [ok] (resolve ok)));TODO check what is returned, can I return the data or just the status
                ))))

(defmethod idb-oper :select [k]
  (let [object-store (:object-store k)
        where (:where k)
        update (:update k)
        delete? (:delete k)
        descending? (:descending k)
        index-column (:index-column k)
        index-name (when index-column (get-index-name index-column (:name k)))
        raw? (:raw k)
        start-pos (:start k);start the fetching from
        num-rows (:rows k);number of rows fetched
        fetched (atom 0)
        pos-atom (atom -1)
        ;cursor advancing is done with callback, so this is the only way to keep the state
        ]
    (assert (if delete? (nil? update) true) "Both delete and update specified")
    (p/get-promise (fn [resolve reject]
                (let [rez #js []
                      crsr-req (if index-name
                             (.openCursor (.index object-store index-name) (when descending? "prev" "next"))
                             (.openCursor object-store (when descending? "prev" "next")))]
                  (set! (.. crsr-req -onsuccess)
                        (fn [evt]
                          (let [cursor (.. evt -target -result)]
                            (if (or (not cursor)
                                    (and num-rows (= num-rows @fetched)))
                              (resolve (if raw? rez (js->clj rez)))
                              (let [_curr_  (.-value cursor)
                                    _curr (if raw? _curr_ (js->clj _curr_  :keywordize-keys true))]
                                (if (and where (-> _curr where not))
                                  (.continue cursor)
                                  (let [curr (if update (update _curr) _curr)]
                                    (if delete?
                                      (.delete cursor)
                                      (when update
                                        (.update cursor curr)
                                        ))
                                    (swap! pos-atom inc)
                                    (when-not (< @pos-atom start-pos) (swap! fetched inc)   (.push rez curr))
                                  
                                    (.continue cursor)))))))))))))

(defmethod idb-oper :select-all [k]
  (let [object-store (:object-store k)
        raw? (:raw k)]
    (p/get-promise (fn [resolve reject]
                (let [rez #js []]
                  (set! (.. object-store (openCursor) -onsuccess)
                        (fn [evt]
                          (let [cursor (.. evt -target -result)]
                            (if-not cursor
                              (resolve (if raw? rez (js->clj rez)))
                              (let [_curr (.-value cursor)
                                    curr (if raw? _curr (js->clj _curr  :keywordize-keys true))]
                                (.push rez curr)
                                (.continue cursor)
                                ))))))))))

(defmethod idb-oper :update [k]
  (idb-oper (assoc k :type :select)))

(defmethod idb-oper :delete [k]
  (idb-oper (assoc k :type :select :delete true)))

(defn transaction-promise
  "transaction fires just once in ddl (probably also in dml)"
  [tx]
  (p/get-promise (fn [resolve reject]
              (set! (.-oncomplete tx)
                    (fn [evt] 
                      (resolve evt)))
              (set! (.-onerror tx)
                    (fn [err]
                      (.log js/console err)
                      (reject err)))
              (set! (.-onblocked tx)
                    (fn [err]
                      (.log js/console "transaction blocked")
                      (.log js/console err)
                      (reject err))))))

(defn ddl
  [objects & ops]
 (let [raw? (first ops)
        upgrade-promise (atom nil)
        upgrade-fn
        (fn [evt]
          (swap! upgrade-promise 
                 (fn [_]
                   (let [db (-> evt (aget "target") (aget "result"))
                         tran-map
                         (doall (map (fn [o] (idb-oper (assoc  o :database db))) objects))]
                     (transaction-promise (last tran-map))))))
        get-inc-database-version 
        (fn []
          (p/get-promise (fn [resolve reject]
                      (let [req (.open js/indexedDB @databaseName)
                            old-version (atom nil)]
                        (.log js/console "opening inc database")
                        (set! (.-onupgradeneeded req)
                              (fn [evt]
                                (.log js/console "onupgradeneeded get-inc-db-version")
                                (swap! old-version (fn [_] (-> evt (aget "oldVersion"))))
                                (upgrade-fn evt)
                                (.log js/console "onupgradeneeded get-inc-db-version finished")
                                ))
                        (set! (.-onsuccess req) 
                              (fn [evt]
                                (let [db (-> evt (aget "target") (aget "result"))]
                                  (set! (.-onversionchange db)
                                        (fn[evt]
                                          (.log js/console "!!! closing the DB onversionchange get-inc-database")
                                          (.. evt -target (close))))
                                  )
                                (.log js/console "get-inc-database-version success")
                                
                                (if @old-version (resolve @old-version)
                                    (resolve (-> evt (aget "target") (aget "result") (aget "version"))))))
                        (set! (.-onerror req) 
                              (fn [evt]
                                (.log js/console "get-inc-database-version error")
                                (reject (-> evt (aget "target") (aget "errorCode")))
                                ))
                        (set! (.-onblocked req)
                              (fn [blocked]
                                (.log js/console "get-inc-database-version blocked")
                                (reject blocked)))))))
        prom 
        (->
         @curr-idb-oper
         (p/then (fn [_]
                 (.log js/console "starting ddl")
                 (.log js/console objects)
                 (get-inc-database-version)))
         (p/then (fn [version]
                 (if (= 0 version) 
                   @upgrade-promise 
                   (p/get-promise (fn [resolve reject]
                               (let [req (.open js/indexedDB @databaseName (inc version))]
                                 (set! (.-onerror req)
                                       (fn [evt]
                                         (.log js/console "ddl on error")
                                         (reject (-> evt (aget "target") (aget "errorCode")))))
                                 (set! (.-onblocked req)
                                       (fn [evt]
                                         (.log js/console "ddl on blocked")
                                         (reject "idb get-inc-db-version ddl blocked")))
                                 (set! (.-onsuccess req)
                                       (fn [evt]
                                         (.log js/console "ddl success")
                                         (let [db (-> evt (aget "target") (aget "result"))]
                                           (set! (.-onversionchange db)
                                                 (fn[evt]
                                                   (.log js/console "!!! closing the DB onversionchange get-inc-database")
                                                   (.. evt -target (close))))
                                           )
                                         (resolve @upgrade-promise)
                                         ))
                                 (set! (.-onupgradeneeded req)
                                       (fn [evt]
                                         (.log js/console "onupgradeneeded ddl")
                                         (upgrade-fn evt))
                                       ))))))))]
   (swap! curr-idb-oper (fn [_] (p/then-catch prom (fn [err]
                                                   (.log js/console "error in idb caught:")
                                                   (.log js/console err)
                                                   ))))
    prom))

(defn dml
  [operations & ops]

  (let [raw? (first ops)
        read-only? (second ops)
        table-names (clj->js (map :name operations))
        dmlo (fn [tx]
                 (fn [operation]
                   (let [object-name (:name operation)]
                     (let [object-store (.objectStore tx (:name operation))]
                       (idb-oper (assoc operation :transaction tx :object-store object-store :raw raw?))))))
        prom-tx (fn []
                  (.log js/console "starting transactions for " table-names)
                  (->
                   (get-database)
                   (p/then (fn [db] (p/get-resolved-promise (.transaction db table-names (if read-only? "readonly" "readwrite")))))
                   (p/then (fn [tx]
                           (let [all-dmlos 
                                 (let [dmlox (dmlo tx)]
                                   (p/prom-all (doall (map dmlox operations))))]
                             (p/get-promise (fn [resolve reject]
                                         (set! (.-oncomplete tx)
                                               (fn [evt] 
                                                 (.log js/console "dml transaction oncomplete")
                                                 (p/then all-dmlos 
                                                        (fn [ok]
                                                          (.log js/console "dml transaction finished")
                                                          (if raw?
                                                            (resolve ok)
                                                            (resolve (clj->js ok)))))))
                                         (set! (.-onerror tx)
                                               (fn [err] 
                                                 (.log js/console "dml trnasaction error")
                                                 (reject err)))
                                         (set! (.-onblocked tx)
                                               (fn [err] 
                                                 (.log js/console "dml trnasaction blocked")
                                                 (reject err))))))))))
        prom (->
              @curr-idb-oper
              (p/then (fn [_]
                      (.log js/console  "starting dml for ")
                      (.log js/console (clj->js (map :name operations)))
                      (.log js/console (clj->js (map :type operations)))
                      (exist-object-stores? (map :name operations))));if there are no tables, let go the curr-idb-oper so the ddl can finish
              (p/then (fn [ex?]
                      (if ex?
                        (prom-tx)
                        (do
                          (.log js/console "************objectstore doesn't exist")
                          (.log js/console  (clj->js (map :name operations)))
                          (p/get-rejected-promise "objectstore doesn't exist"))))))
        ]
    (swap! curr-idb-oper (fn [_] (p/then-catch prom (fn [err]
                                                   (.log js/console "error in idb caught:")
                                                   (.log js/console err)
                                                   ))))
    prom))

(defn ^:export drop-database
  []
  (p/get-promise (fn [resolve reject]
              (let [req (.deleteDatabase js/indexedDB @databaseName)]
                (set! (.-onsuccess req)
                      (fn [ok]
                        (.log js/console "database dropped successfully")
                        (resolve ok)))
                (set! (.-onblocked req)
                      (fn [blocked]
                        (.log js/console "database deletion blocked")
                        (reject blocked)))
                (set! (.-onerror req)
                      (fn [err]
                        (.log js/console "database deletion error")
                        (reject err)))))) "drop-databases"
  )

(defn test-idb
  []
  (drop-database)
  (ddl
   [{:type :create :name "radnici" :key "id" }
    {:type :create :name "stolovi" :key "telefon"}
    ])
  (dml
   [{:type :insert :name "radnici" :data [{:id 1 :ime "dusan" :prezime "miloradovic"}
                                          {:id 2 :ime "mohammad" :prezime "minhaj" }]}
    {:type :insert :name "stolovi" :data {:telefon 4567 :model "ramdea" :proizvodjac "ikea"}}
    {:type :select-by-key :name "radnici" :key 1}
    
    ]
   )
  (ddl
   [{:type :create :name "proba" :key "id"}
    ]
   )
  (dml
   [{:type :insert :name "proba" :data {:id 1 :bla "bla"}}]
   )
  )

(defn test-idb2
  []
  (dml
   [{:type :select :name "radnici" :where (fn [r] (= (:prezime r) "miloradovic"))}])
  )
