(ns maximoplus.db)

(defmacro dml1
  [statement & args]
  `(maximoplus.promises.then
    (maximoplus.db.dml [~statement] ~@args)
    (fn [res#] (first res#))))
