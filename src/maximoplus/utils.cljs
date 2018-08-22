(ns maximoplus.utils
  (:require 
   [goog.json]
   [maximoplus.arrays :as ar]         
   [cognitect.transit :as transit]
   )
  )

(defn debug-exception [e]
  (when js/console
    (if (.-debug js/console)
      (.debug js/console e)
      (.log js/console e))))

(defn ^:export debug [& x]
  (when js/console
    (if (.-debug js/console)
      (.debug js/console (apply str x))
      (.log  js/console (apply str x)))))



(defn error [& x]
  (when js/console
    (if (.-error js/console)
      (.error js/console (apply str x))
      (.log js/console (apply str x)))))

(defn ddebug [& x] 
  (when js/console
    (if (.-debug js/console)
      (.debug js/console (apply str x))
      (.log js/console (apply str x)))))


(defn read-json [x]
  ;creates JS object for performance
  (if js/JSON
    (.parse js/JSON x)
    (goog.json/parse x)))

(defn create-json [x]
  (if js/JSON
    (.stringify js/JSON x)
    (goog.json/serialize x)))

(defn create-cljs-json [x]
  (-> x clj->js create-json)
)

(defn clj-prstr [x]
  (-> x clj->js pr-str)
)

(defn print-json-from-arr [& x]
  (apply str (map create-json (map #(if (nil? %) "null" %)(clj->js  (if (nil? x) "null" x))))))

(def transit-writer (transit/writer :json))
(def transit-reader (transit/reader :json))

(defn transit-json [x]
  ;use transit to create json from clojurescript object
  (transit/write transit-writer x)
  )

(defn transit-read [x]
  (transit/read transit-reader x))

(defn transit-encode-uri [x]
  (js/encodeURIComponent (transit-json x)))


;(defn transit-encode-uri [x]
;  (pr-str x));test compilation

(defn array-to-obj
  [arr]
  (when arr
    (let [_ln (.-length arr)
          ret (js/Object.)]
      (loop [ind 0]
        (if (== ind _ln)
          ret
          (do
            (aset ret (aget arr ind) (aget arr (inc ind)))
            (recur (+ ind 2))))))))

(defn vec-to-map
  [v]
  (into {} (map vec (partition 2 v))))

(defn find-in-arr
  [arr f]
  (let [ln (.-length arr)]
    (loop [ind 0]
      (when-not (== ind ln)
        (let [curr (aget arr ind)]
          (if (f curr)
            [curr ind]
            (recur (inc ind))))))))

(defn first-in-arr
  [arr f]
  (first (find-in-arr arr f)))

(defn first-ind-in-arr
  [arr f]
  (second (find-in-arr arr f)))

(defn encodeURIComponents
  [args]
  (js/encodeURIComponent (print-json-from-arr  (if (nil? (last args) )(butlast args) args))))

(defn arr->vector [arr]
  (let [len (.-length arr)]
    (loop [i 0 vec []]
      (if (>= i len)
        vec
        (recur (inc i) (conj vec (aget arr i)))))))

(defn vector->arr [_vc]
  (let [len (count _vc)
        arr (ar/empty)]
    (loop [i 0]
      (if (>= i len)
        arr
        (do 
          (ar/conj! arr (get _vc i))
          (recur (inc i)))))))

(defn clone-object [obj]
  (let [obj-keys (js-keys obj)
        new-obj (js-obj)]
    (dotimes [i (.-length obj-keys)]
      (let [key (aget obj-keys i)
            val (aget obj key)]
        (aset new-obj key val)))
    new-obj))

(defn state-clone
  [obj]
  (cond
    (array? obj)
    (let [arr-length (ar/count obj)
          new-array (js/Array. arr-length)]
      (dotimes [i arr-length]
        (ar/assoc! new-array i (state-clone (ar/nth obj i))))
      new-array)

    (identical? (type obj) js/Object);;so complex types will not be marked as objects
    (let [obj-keys (js-keys obj)
          new-obj (js-obj)]
      (dotimes [i (.-length obj-keys)]
        (let [key (aget obj-keys i)
              val (state-clone (aget obj key))]
          (aset new-obj key val)))
      new-obj)

    :else obj
    )
  
  );;this clones the object and arrays recursively. everything else is left as it is (will not copy complex data and functions)

;;instead of goog/inerits (goog.base not working properly)
(defn inherits
  [ch-type parent-type]
  (when (.-prototype parent-type)
    (set! (.. ch-type -prototype) (.create js/Object (.. parent-type -prototype)))))
