(ns helins.fcss.dev.css

  ""

  {:author "Adam Helinski"}

  (:require [helins.fcss           :as fcss]
            [helins.fcss.dev.css-2 :as fcss.dev.css-2]))


;;;;;;;;;;


(fcss/defclass klass)

(fcss/defvar bg)



(fcss/defrul rule+

  ["&"
   "body"
   {:background (fcss/templ bg)
    bg          fcss.dev.css-2/color}]

  [klass
   {:color 'red}])
