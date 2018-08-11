(ns maximoplus.idb
  )

;new version of macro doesn't handle the rejection, it is up to the client
(defmacro idb-do
  [& args]
  `(maximoplus.idb.set-curr-idb-oper!
    ~(loop [ar (rest args) res `(.then @maximoplus.idb.curr-idb-oper (fn [_#] ~(first args)))]
       (if (empty? ar)
         `(.thenCatch ~res
                      (fn [err#]
                        (.log js/console err#)
;                        (maximoplus.idb.set-curr-idb-oper! (.resolve goog.Promise) "start")
                        ;(.reject goog.Promise err#)
                        (.resolve goog.Promise "error")
                        ))
         (recur (rest ar)
                `(.then ~res (fn [ok#] ~(first ar))))))))


(defmacro idb->
  [& args]
  (loop [ar (rest args) res (first args)]
    (if (empty? ar)
      res
      (recur (rest ar)
             `(.then ~res (fn [ok#] ~(first ar)))
             ))))

;this macro except just one promise as an argument and wraps it
(defmacro idb-atomic
  [promise]
  `(maximoplus.idb.set-curr-idb-oper!
    (.thenCatch
     (.then @maximoplus.idb.curr-idb-oper (fn [_#] ~promise))
     (fn [err#]
       (.log js/console err#)
       (.resolve goog.Promise "error")))
    )
  )

(defmacro dml1
  [statement & jsro?]
  (let [js? (first jsro?)
        ro? (second jsro?)]
    `(.then
      (maximoplus.idb.dml [~statement] ~js? ~ro?)
      (fn [res#] (first res#)))))
