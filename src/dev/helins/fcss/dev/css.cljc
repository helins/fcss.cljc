;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.fcss.dev.css

  ""

  {:author "Adam Helinski"}

  (:require         [garden.units          :as css.unit]
                    [helins.fcss           :as fcss]
            #?(:clj [helins.fcss.compiler])
                    [helins.fcss.dev.css-2 :as fcss.dev.css-2]))


;;;;;;;;;;


#?(:clj (println :clj-1))



(fcss/defdata data-x)



(defn rul-coll

  ""

  []

  (map identity
       [["foo"
         {:color 'yellow}]
        [["bar"
          "baz"]
         {:position 'absolute}]
        [["bar[&=true]"
          "baz[&=true]"]
         data-x
         {:position 'relative}]]))



(fcss/defclass klass

  "Test"

  (let [color 'red]
    [klass
     {:color      color
      :position   'relative
      :top        0
      :left       0
      :display    'flex
      :background 'purple}])

  (rul-coll))



(fcss/defvar bg)



(fcss/defvar length

  :fallback (css.unit/px 42)

  ["test"
   {length "101px"
    :left  [length]}])
 
 

(fcss/defany rul-1

  "Docstring test.

   Multi-line.
   Foo bar"

  ["&"
   "body"
   {:background [bg]
    :left       ["calc(1 * &)"
                 (fcss/fallback length
                                "100%")]
    :top        (css.unit/percent 50)
    :width      [length]
    bg          fcss.dev.css-2/color
    length      "50px"}])



(fcss/defanim anim

  "Test"

  ["0%" {:opacity 0}]
  (list ["50%" {:opacity 0.5}]
        ["75%" {:opacity 0.75}])
  ["100%" {:opacity 1}])
