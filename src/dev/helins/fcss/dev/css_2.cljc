(ns helins.fcss.dev.css-2

  ""

  (:require [helins.fcss :as fcss]))


;;;;;;;;;;


(def color 'green)



(fcss/defclass klass-2)



(fcss/defrul my-rule

  [klass-2
   {:color 'orange}])
