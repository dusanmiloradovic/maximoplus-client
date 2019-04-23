(ns maximoplus.promises)


(defprotocol Deferred
  (callback [this] [this val])
  (errback [this] [this val])
  (promise [this])
  (then [this cb])
  (then-catch [this cb])
  (has-fired? [this])
  )


(defn get-specified
  [promise]
  (specify! promise
    Deferred
    (then
      [this cb]
      (get-specified (.then promise cb)))
    (then-catch
      [this errb]
      (get-specified (.catch promise errb)))
    (has-fired?;;when it is called it assumes the deferred. If there is a Promise, means that we already passed the initial stage
      [this]
      true)))

(defn get-deferred
  []
  (let [o (js/Object.)
        p (js/Promise.
           (fn [resolve reject]
             (set! (.-resolve o) resolve)
             (set! (.-reject o) reject)
             ))]
    (set! (.-promise o) p)
    (set! (.-fired o) false)
    (specify! o
      Deferred
      (promise
        [this]
        p)
      (callback
        ([this]
         (let [r (.-resolve  o)]
           (set! (.-fired o) true)
           (r nil)))
        ([this val]
         (let [r (.-resolve  o)]
           (set! (.-fired o) true)
           (r val))))
      (errback
        ([this]
         (let [r (.-reject o)]
           (set! (.-fired o) true)
           (r nil)))
        ([this val]
         (let [r (.-reject o)]
           (set! (.-fired o) true)
           (r val))))
      (has-fired?
        [this]
        (.-fired o))
      (then
        [this callback]
        (get-specified (.then p callback)))
      (then-catch
        [this errback]
        (get-specified (.catch p errback))))))



(defn get-promise
 [resolve-fun]
  (get-specified (js/Promise. resolve-fun))) ;;resolve fun is (fn[resolve reject[....)

(defn get-resolved-promise
  [value]
  (get-specified (.resolve js/Promise value)))

(defn get-rejected-promise
  [value]
  (get-specified (.reject js/Promise value)))

(defn prom-all
  [values]
  (get-specified (.all js/Promise (clj->js values))))
