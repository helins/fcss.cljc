(ns helins.fcss.dev.css-2

  ""

  (:require [garden.core       :as css]
            [garden.stylesheet :as css.stylesheet]
            [helins.fcss       :as fcss]))


;;;;;;;;;;

#?(:clj (println :clj-2))


(def color 'green)


(fcss/defclass klass-2)


(fcss/defrul my-rule

  [klass-2
   {:color 'orange}])


(fcss/defdata data-foo

  ""

  ["*[&=true]"
   data-foo
   {:color 'red}])



(fcss/defclass klass-3

  (list (css.stylesheet/at-media {:screen :only}
                                 ["$class[$data=true]"
                                  {:class klass-3
                                   :data  data-foo}
                                  {:background 'green}])))
