;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.fcss.dev.css-2

  ""

  (:require #?(:clj [garden.color      :as css.color])
            #?(:clj [garden.stylesheet :as css.stylesheet])
            [helins.fcss               :as fcss]))


;;;;;;;;;;


#?(:clj (println :clj-2))


(def color 'green)


(fcss/defclass klass-2

  [klass-2
   {:background :pink}])


(fcss/defany my-rule

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
                                  {:background (css.color/hsl 150 0.5 0.5)}])))
