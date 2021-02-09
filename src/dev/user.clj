(ns user

  "For daydreaming at the REPL."

  (:require [clojure.java.io :as io]
            [clojure.pprint]
            [clojure.string]
            [helins.fcss     :as fcss]
            [kaocha.repl     :as kaocha.repl]))
        

;;;;;;;;;;


(comment


  (do
    (require '[vlaaad.reveal :as reveal])
    (def reveal
         (reveal/ui))
    (add-tap reveal))

  (remove-tap reveal)



  (let [tty (io/writer "/dev/pts/5")]
    (binding [*err* tty
              *out* tty]
      (println \newline)
      (kaocha.repl/run)))




  ;; For parsing table from https://www.w3.org/Style/CSS/all-properties.en.html
  ;;
  ;;
  (clojure.pprint/pprint (into (sorted-set)
                               (comp (map (comp keyword
                                                second
                                                #(clojure.string/split %
                                                                       #"\s")))
                                     (filter some?))
                              (line-seq (io/reader "PROPS.txt"))))


  

  )
