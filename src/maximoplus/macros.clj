(ns maximoplus.macros
  (:use clojure.walk)
  (:use     [cljs.core :only [this-as]])
  (:require [clojure.string  :refer [join]])
  )


                                        ;ova verzija radi 2 stvari: moguce je definisati cb i errcb funkciju prilikom definisanja, a pritom se pravi funkcija sa opcionim argumentom. To znaci da mogu da definisem komandu bez default callback-a koja moze da se poziva sa dodatinim callbackom kada je to potrebno
(defmacro defcmd-all [name args post? & actions]
  (let [y (str name)
        z (str y " got ok with the result")
        opt (gensym)
        uexc (gensym)]
    `(defn ~name [~@args & ~opt]
       (let [c# (str "command=" ~y "&data=")
             ~uexc (js/Error.)]
         (maximoplus.core/yesnocancelproxy
          (str c# (maximoplus.utils.transit-encode-uri ~args))
          ~(if (first actions)
             `(fn [e#]
                (~(first actions) e#)
                (when (first ~opt)
                  ((first ~opt) e#)))
             `(fn [evt#]
                (maximoplus.utils/debug ~z evt#)
                (when (first ~opt)
                  ((first ~opt) evt#))))
          ~(if (second actions)
             `(fn[e1#]
                (maximoplus.utils/debug-exception ~uexc)
                (if (= (nth (nth e1# 0) 1)  "Not logged in")
                  (maximoplus.core/page-init)
                  (do
                    (~(second actions) e1#)
                    (if (second ~opt)
                      ((second ~opt) e1#)
                      (maximoplus.core/exc-handler ~y e1#)))))
             `(fn[e2#]
                (maximoplus.utils/debug-exception ~uexc)
                (if (= (nth (nth e2# 0) 1)  "Not logged in")
                  (maximoplus.core/page-init)
                  (do
                    (if (second ~opt)
                      ((second ~opt) e2#)
                      (maximoplus.core/exc-handler ~y e2#)
                      )))))
          ~post?)))))

(defmacro defcmd [name args & actions]
  `(defcmd-all ~name ~args false ~@actions)
  )

(defmacro defcmd-post [name args & actions]
  `(defcmd-all ~name ~args true ~@actions)
  )

                                        ;if the last parameter is null it will not send it, this is used now just for register-mbo-command and registrer-mboset-command



(defmacro defcmd-with-prepare [name args & actions]
  (let [y (str name)
        z (str y " got ok with the result")
        opt (gensym)
        ]
    `(defn ~name [~@args & ~opt]
       ~(when-let [pac#  (first actions)]
          `~(first actions)
          )
       (let  [c# (str  "command=" ~ y "&data=")]
         (maximoplus.core/yesnocancelproxy
          (str c# (maximoplus.utils.transit-encode-uri ~args))
          ~(if (second actions)
             `(fn [e#]
                (~(second actions) e#)
                (when (first ~opt)
                  ((first ~opt) e#)
                  )
                )
             `(fn [evt#]
                                        ;               (maximoplus.utils/debug ~z evt#)
                (when (first ~opt)
                  ((first ~opt) evt#)
                  )
                )
             )
          ~(if (nth actions 2 nil)
             `(fn[e1#]
                (~(nth actions 2) e1#)
                (if (nth ~opt 1)
                  ((nth  ~opt 1) e1#)
                  (maximoplus.core/exc-handler ~y e1#)
                  )
                )
             `(fn[e2#]
                (if (nth ~opt 1)
                  ((nth ~opt 1) e2#)
                  (maximoplus.core/exc-handler ~y e2#)
                  )))))))
  )
                                        ;isto kao komanda samo se prvo izvrsi prepare akcija pa onda se zove komanda preko mreze.
                                        ;Za sada cu specificno ovo da uradim samo za set-value

(defn- to-property [sym]
  (symbol (str "-" sym)))

(defn to-camel-case [x] (clojure.string/replace x #"-(\w)"
                                                #(.toUpperCase (second %))))

(defmacro custom-this []
  (list 'js* "this")
  )

(defmacro test-comp
  [tsym tfields bsym]
  `(defn ~tsym [~@tfields]
     ~(let [thsym (gensym "this") ]
        `(this-as ~thsym
           (goog/base (custom-this))))))


;;goog/base will not be used, instead function that reads the class from the prototype and calls it
(defmacro googbase
  [& params]
  `(.call (aget ~(first params) "_sc_") ~@params)) ;;this should be the first argument to call

(defmacro simple-inherits
  "to overcome problems with closure compiler, give up on goog.base and goog.inherits, just use this, it save the reference to superclass"
  [thsym tsym bsym]
  `(do
     (aset ~thsym "_sc_" ~bsym)))

(defmacro def-comp
  "tsym is a symbol,  bsym components which is being extended. impl are protocol implementations"
  [tsym tfields bsym & impls]
  (let [my-ns (->  *ns* ns-name str)
        thsym (gensym "this")]
    `(do
       (defn ~(vary-meta tsym assoc :jsdoc ["@constructor"] :export true)  [~@tfields]
         ~(if (symbol? (first impls))
            (do
              `(this-as ~thsym
                 (simple-inherits ~thsym ~tsym ~bsym)
                 ~@(map (fn [c] `(aset ~thsym ~(str c) ~c )) tfields)
                 ~thsym)) 
            (let [[constr-name params & bd] (first impls)
                  mta (meta constr-name)
                  ovrrd (when mta (= 'override (:tag mta)))
                  ]
              `(this-as ~thsym
                 (simple-inherits ~thsym ~tsym ~bsym)
                 ~@(map (fn [c] `(aset ~thsym ~(str c) ~c )) tfields)
                 ~@bd
                 ~thsym))))
       (maximoplus.utils/inherits ~tsym ~bsym)
       ~@(let [rep-map (zipmap tfields (map gensym tfields))]
           (map (fn[frm]
                  (let [[mn parms & bd] frm
                        thsym (gensym "this");this# is not working for nested in the macro
                        mta (meta mn)
                        ovrrd (when mta (= 'override (:tag mta)))
                        just-base (when mta (= 'base (:tag mta)))
                        ]
                    `(do
                       ~(when-not just-base
                          `(set! (.. ~tsym -prototype ~(to-property (to-camel-case mn)))
                                 (fn [~@(rest parms)]
                                   (this-as ~thsym
                                     (let [~@(interleave
                                              (map rep-map tfields)
                                              (map (fn[prop]
                                                     `(aget ~thsym ~(str prop)))
                                                   tfields))]
                                       ~(when-not ovrrd
                                          `(when  (.. ~bsym -prototype ~(to-property (to-camel-case mn)))
                                             (.call (~(symbol (str ".-" (to-camel-case mn))) (.-prototype ~bsym)) ~thsym ~@(rest parms))))
                                       ~@(prewalk-replace {(first parms) thsym} bd))))))
                       (goog/exportSymbol  ~(str my-ns "."(str tsym) ".prototype." (to-camel-case mn)) 
                                           (.. ~tsym -prototype ~(to-property (to-camel-case mn)))))));replace first parameter (this) with the actual js/this
                (filter #(-> % symbol? not)
                        (if-not (symbol? (first impls)); if it is not a symbol, it is a constructor, skip
                          (rest (prewalk-replace rep-map impls))
                          (prewalk-replace rep-map impls)))))
       (extend-type
           ~tsym
         ~@(map (fn [el]
                  (if (symbol? el)
                    el
                    (let [[mn params bd] el]
                      `(~mn ~params
                        (.call (aget (custom-this) ~(str (to-camel-case mn)))   ~(first params) ~@(rest params))))))
             (if (symbol? (first impls))
               impls
               (rest impls)))))))

(defmacro p-deferred [o & bd]
  `(if-let [prom-chan# (maximoplus.basecontrols.get-deferred ~o)]
     (maximoplus.core.simple-receive
      prom-chan# (fn [_#]
                   ~@bd
                   ))
     (do ~@bd)))


(defmacro p-deferred-on [o & bd]
  `(maximoplus.core.simple-receive ~o (fn [_#] ~@bd))
  )


(defmacro p-deferred-on-attr [o attr & bd]
  `(p-deferred-on (aget ~o ~attr) ~@bd))


(defmacro loop-arr [[symbol arr] & body]
  `(when-let  [arrmut# ~arr];mutable, so not possible to read directly
     (let [ln# (.-length arrmut#)]
       (dotimes [i# ln#]
         (let [~symbol (aget arrmut# i#)]
           ~@body)))))

(defmacro c! [control command cont-f & args] ;this will be used in cases where the function called already calls the kk! macro
  `(let [cb-handler# (maximoplus.basecontrols.get-callback-handler ~control)
         err-handler# (maximoplus.basecontrols.get-errback-handler ~control)
         fh# (maximoplus.basecontrols.get-finish-call-handler ~control)
         ]
     ((maximoplus.basecontrols.get-prepare-call-handler ~control) ~command)
     (~cont-f  ~@args
      (fn [ok#]
        (fh# ~command)
        (cb-handler# ~command ok#))
      (fn [err#]
        (fh# ~command)
        (err-handler# ~command err#)))))

(defmacro p! [container command cont-f & args]
  `(let [fh# (maximoplus.baasecontrols.get-finish-call-handler ~container)]
     ((maximoplus.basecontrols.get-prepare-call-handler ~container) ~command)
     (maximoplus.promises.get-promise
      (fn [resolve# reject#]
        (~cont-f (.getId ~container) ~@args
         (fn [ok#]
           (fh# ~command)
           (resolve# ok#))
         (fn [err#]
           (fh# ~command)
           (reject# err#)))))))


(defmacro kk!
  [container command cont-f & args]
  (let [errh (last args);;user can pass null to the container function calling kk!, so we need to check
        cbh (-> args butlast last)
        args-rest (-> args butlast butlast)]
    `(js/Promise.
      (fn [resolve# reject#]
        (let [[pch# cbh# errbh#] (maximoplus.basecontrols.get-callbacks ~container ~command  ~cbh ~errh resolve# reject#)
              fs# (fn [_cbh# _errbh#] ;; I will send the closure to the channel for the execution, the idea is to avoid stack overflow errorrs (mainly in nodejs). This will be stored on heap. the get-callbacks function will resolve the promise if user needs to use it (maybe this is not necessary, I will see after the testing)
                    (pch# ~command)
                    (try
                      (~cont-f (.getId ~container) ~@args-rest _cbh# _errbh#)
                      (catch :default e#
                        (_errbh# e#))))]
          (maximoplus.core/send-command ~container ~command  fs# cbh# errbh#))))))


(defmacro k!;;like kk! witthout the callback and errback (get default from container)
  [container command cont-f & args]
  `(let [cbh# (maximoplus.basecontrols.get-callback-handler ~container)
         erbh#  (maximoplus.basecontrols.get-errback-handler ~container)]
     (kk! ~container ~command ~cont-f ~@args
          cbh#
          erbh#
          )))

(defmacro kc!;;it will call kk! with the default callback and errback handler from the control
  [control command cont-f & args]
  `(let [cont# (maximoplus.core.get-container ~control)
         cbh# (maximoplus.basecontrols.get-callback-handler ~control)
         erbh#  (maximoplus.basecontrols.get-errback-handler ~control)]
     (kk! cont# ~command ~cont-f ~@args
          cbh#
          erbh#)))


(defmacro kk-branch! [orig-cont container command cont-f & args]
  `(kk! ~container ~command ~cont-f ~@args);;since we don't need to set the promises anymore, this function is superflous
  )

(defmacro kk-nocb!
  [container command cont-f & args]
  `(js/Promise.
    (fn [resolve# reject#]
      (let [[pch# cbh# errbh#] (maximoplus.basecontrols.get-callbacks ~container ~command  nil nil resolve# reject#)
            fs# (fn [_cbh# _errbh#] ;; I will send the closure to the channel for the execution, the idea is to avoid stack overflow errorrs (mainly in nodejs). This will be stored on heap. the get-callbacks function will resolve the promise if user needs to use it (maybe this is not necessary, I will see after the testing)
                  (pch# ~command)
                  (try
                    (~cont-f (.getId ~container) ~@args _cbh# _errbh#)
                    (catch :default e#
                      (_errbh# e#))))]
        (maximoplus.core/send-command ~container ~command fs# cbh# errbh#)))))


(defmacro kk-control-nocb!
  [control container command control-f & args]
  `(let [fh# (maximoplus.basecontrols.get-finish-call-handler ~control)
         cbh# (maximoplus.basecontrols.get-callback-handler ~control)
         errbh# (maximoplus.basecontrols.get-errback-handler ~control)]
     (js/Promise.
      (fn [resolve# reject#]
        (maximoplus.core/send-command
         ~container
         ~command
         (fn [_cbh# _errbh#]
           (try
             (~control-f  (.getId ~container) ~@args
              (fn [ok#]
                (fh# ~command)
                (_cbh# ok#)
                (.call resolve# nil ok#))
              (fn [err#]
                (fh# ~command)
                (_errbh# err#)
                (.call reject# nil err#)))
             (catch :default e#
               (fh# ~command ~control)
               (_errbh# e#)
               (.call reject# nil e#)
               )))
         cbh# errbh#)))))



                                        ;here the deferred will be in the queue in orig-container queues, so the operation will be serialized. the new branch stems from the point and operations can be parallel after that(doesn't need to wait for the server response of the original branch)


(defmacro kk-branch-nocb!
  [orig-cont container command cont-f & args]
  `(kk-nocb! ~container ~command ~cont-f ~@args))



(defmacro kk-branch-noargs!
  [orig-cont container command cont-f & args]
  (let [errh (last args)
        cbh (-> args butlast last)]
    `(js/Promise.
      (fn [resolve# reject#]
        (let [[pch# cbh# errbh#] (maximoplus.basecontrols.get-callbacks ~container ~command  ~cbh ~errh resolve# reject#)
              fs# (fn [_cbh# _errbh#]
                    (pch# ~command)
                    (try
                      (~cont-f _cbh# _errbh#)
                      (catch :default e#
                        (_errbh# e#))))]
          (maximoplus.core/send-command ~container ~command fs# cbh# errbh#))))))

(defmacro offline-alt
  [fun-name  online-cmd offline-cmd & _args]
  (let [opt (gensym)
        args (first _args)]
    `(defn ~fun-name [~@args & ~opt]
       (let [cb# (first ~opt)
             errb# (second ~opt)]
         (if @maximoplus.core/is-offline
           (let [obj-name#  (aget maximoplus.core/rel-map ~(first args))]
             (~offline-cmd ~(first args) obj-name# ~@(rest args) cb# errb#))
           (~online-cmd ~@args cb# errb#))))))

(defmacro offline-alt-noobj
  "for the offline functions where objectname is not yet mapped to the control name, ie. for the registering"
  [fun-name  online-cmd offline-cmd & _args]
  (let [opt (gensym)
        args (first _args)]
    `(defn ~fun-name [~@args & ~opt]
       (let [cb# (first ~opt)
             errb# (second ~opt)]
         (if @maximoplus.core/is-offline
           (~offline-cmd ~@args cb# errb#)
           (~online-cmd ~@args cb# errb#)))))
  )

(defmacro ..prom
  [errb & args]
  `(maximoplus.promises.then-catch
    (..
     ~@args
     )
    (fn [err#] (when ~errb (~errb err#)) err#)))

                                        ;(defmacro ..unique-conts
                                        ;  [container cnt-symbol & args]
                                        ;  (let [pre-process (first args)
                                        ;        main-process (rest args)]
                                        ;    `(..
                                        ;      (.getRowCount container)
                                        ;      (then
                                        ;       (fn [_cnt#]
                                        ;         (let [cnt# (aget _cnt# 0)]
                                        ;           (.then (.fetchData ~container 0 cnt#) (fn [_] cnt#)))))
                                        ;      (then
                                        ;       (fn [cnt#]
                                        ;         (loop [i# 0 rez# (.resolve goog.Promise "start")]
                                        ;           (if (>= i# cnt#)
                                        ;             rez#
                                        ;             (..
                                        ;              (let [_uid# (aget (maximoplus.core.get-local-data ~container i#) "_uniqueid")
                                        ;                    uid# (.toString _uid#)
                                        ;                    uc# (maximoplus.basecontrols.UniqueMboContainer. (aget ~container "mboname") uid#)
                                        ;                    _ (~pre-process )])
                                        ;              )))))
                                        ;     )
                                        ;    )
                                        ;)


;;simplifies the promise expressions
(defmacro promise-do
  [& args]
  `(do
     ~(loop [ar (rest args) res `(maximoplus.promises.then
                                  (maximoplus.promises.get-resolved-promise "start")
                                  ~(first args))]
        (if (empty? ar)
          `~res
          (recur (rest ar)
                 `(maximoplus.promises.then ~res  ~(first ar)))))))

(defmacro do-offline
  [& args]
  `(do
     ~(loop [ar (rest args) res `(maximoplus.promises.then
                                  (maximoplus.offline.db-ready?)
                                  ~(first args))]
        (if (empty? ar)
          `~res
          (recur (rest ar)
                 `(maximoplus.promises.then ~res  ~(first ar)))))))

                                        ;new version  of offline-do, just the wrapper for offline-f-do

(defmacro offline-do-nw
  [& args]
  `(maximoplus.offline.offline-f-do [~@args]))

(defmacro react-call
  [f-name & args]
  `(when-let [r# (maximoplus.core.get-state (custom-this) :react)]
     (.call (aget r# ~(str f-name)) nil ~@args)))

(defmacro react-call-control
  [control f-name & args]
  `(when-let [r# (maximoplus.core.get-state ~control :react)]
     (.call (aget r#  ~(str f-name)) nil ~@args)))

(defmacro with-react-props
  [command control & args]
  `(~command ~@(map (fn [s]
                      `(aget (aget ~control "props") ~(str s))
                      ) args)))

(defmacro with-react-state
  [command control & args]
  `(~command ~@(map (fn [s]
                      `(aget (aget ~control "state") ~(str s))
                      ) args)))

(defmacro react-prop
  [control prop]
  `(aget (aget ~control "props") ~(str prop))
  )

(defmacro react-update-field
  [[state column bfld] & body]
  `(let [fields# (aget ~state "fields")]
     (loop-arr [~bfld fields#]
               (when (= ~column (aget ~bfld "column"))
                 ~@body
                 ))))

(defmacro react-loop-fields
  [[state bfld] & body]
  `(let [fields# (aget ~state "fields")]
     (loop-arr [~bfld fields#]
               ~@body)))
