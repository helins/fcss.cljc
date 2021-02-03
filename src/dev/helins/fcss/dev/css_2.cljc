(ns helins.fcss.dev.css-2

  ""

  (:require [helins.fcss :as fcss]))


;;;;;;;;;;

#?(:clj (println :clj-2))


(fcss/co-load*)


(def color 'green)



(fcss/defclass klass-2)



(fcss/defrul my-rule

  [klass-2
   {:color 'orange}])
