(ns user

  "For daydreaming at the REPL."

  (:require [clojure.java.io :as io]
            [helins.fcss     :as fcss]
            [kaocha.repl     :as kaocha.repl]
            [vlaaad.reveal   :as reveal]))
        

;;;;;;;;;;


(comment


  (do
    (def reveal
         (reveal/ui))
    (add-tap reveal))

  (remove-tap reveal)



  (let [tty (io/writer "/dev/pts/5")]
    (binding [*err* tty
              *out* tty]
      (println \newline)
      (kaocha.repl/run)))



  )
