(ns helins.fcss.dev.css

  ""

  {:author "Adam Helinski"}

  (:require [garden.units          :as css.unit]
            [helins.fcss           :as fcss]
            [helins.fcss.dev.css-2 :as fcss.dev.css-2]))


;;;;;;;;;;


(fcss/defclass klass)

(fcss/defvar bg)

(fcss/defvar length

  :unit %)
 
 

(fcss/defrul rule+

  ;"Docstring test.

  ; Multi-line.
  ; Foo bar"

  ["&"
   "body"
   {:background bg
    :left       (fcss/templ "calc(1px * var(&, 42))"
                            (str length))
    :top        (css.unit/percent 50)
    :width      length
    bg          fcss.dev.css-2/color
    length      50}]

  (let [color 'red]
    [klass
     {:color color}]))
