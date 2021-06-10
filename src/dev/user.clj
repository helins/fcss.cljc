;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns user

  "For daydreaming at the REPL."

  (:require [clojure.java.io :as io]
            [clojure.pprint]
            [clojure.string]
            #_[helins.fcss.dev]))
        

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
