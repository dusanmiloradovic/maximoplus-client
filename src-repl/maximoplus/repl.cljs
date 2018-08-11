(ns maximoplus.repl
  (:require
   [clojure.browser.repl :as repl]
   [weasel.repl :as wrepl]))

(defn ^:export start-the-repl
  [& address]
  (let [addr (if address (first address) "localhost")
        cstr (str "http://" addr ":9000/repl")]
    (repl/connect cstr))
                                        ;morao sam da stavim u odvojen metod, nesto se desava u advanced modu sto onemogucava start-receiving-event da radi
  )

(defn ^:export start-weasel
  []
  (wrepl/connect "ws://localhost:9001" :verbose true))




