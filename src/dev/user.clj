(ns user

  "For daydreaming at the REPL."

  (:require [clojure.java.io :as io]
            [clojure.pprint]
            [clojure.string]
            [helins.fcss.dev]))
        

;;;;;;;;;;


(comment


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
