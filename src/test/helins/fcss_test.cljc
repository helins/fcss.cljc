(ns helins.fcss-test

  ""

  (:require [clojure.string]
            [clojure.test  :as t]
            [garden.color]
            [garden.units  :as garden.unit]
            [helins.fcss   :as fcss]))


;;;;;;;;;;


(fcss/defclass sel-class)

(fcss/defid sel-id)



(t/deftest color->hex

  (t/is (= "#000000ff"
           (fcss/color->hex (garden.color/hsla 0 0 0 1)))))


(t/deftest templ

  (t/is (= ".class"
           (fcss/templ ".class"))
        "Arity 1 with a string")
  (t/is (= "hsla(0, 0%, 0%, 1)"
           (fcss/templ (garden.color/hsla 0 0 0 1)))
        "Arity 1 with a Garden Color")
  (t/is (= "42px"
           (fcss/templ (garden.unit/px 42)))
        "Arity 1 with a Garden Unit")
  (t/is (= "42"
           (fcss/templ 42))
        "Arity 1 with any object")
  (t/is (= (str \.
                sel-class)
           (fcss/templ sel-class))
        "Arity 1 with a Selector")
  (t/is (= (str ".class:hover "
                \.
                sel-class
                \#
                sel-id)
           (fcss/templ "$1:hover $2$3"
                       {:$1 ".class"
                        :$2 sel-class
                        :$3 sel-id}))
        "Variadic"))





(t/deftest interpolate

  (t/is (= (clojure.string/replace "rgb( calc(0 + (100 - 0) * var(--test-var)),
                                         calc(0 + (110 - 0) * var(--test-var)),
                                         calc(0 + (120 - 0) * var(--test-var)))"
                                   #"\s+"
                                   " ")
           (clojure.string/replace (fcss/interpolate (garden.color/hsl 0 0 0)
                                                     (garden.color/rgb 100 110 120)
                                                     "--test-var")
                                   #"\s+"
                                   " "))
        "With Garden Colors")
  (t/is (= (clojure.string/replace "rgba( calc(0 + (100 - 0) * var(--test-var)),
                                          calc(0 + (110 - 0) * var(--test-var)),
                                          calc(0 + (120 - 0) * var(--test-var)),
                                          calc(1 + (0.5 - 1) * var(--test-var)))" 
                                   #"\s+"
                                   " ")
           (clojure.string/replace (fcss/interpolate (garden.color/hsl 0 0 0)
                                                     (garden.color/rgba 100 110 120 0.5)
                                                     "--test-var")
                                   #"\s+"
                                   " "))
        "With alpha Garden Colors")
  (t/is (= "calc(10px + (2em - 10px) * var(--test-var))"
           (fcss/interpolate (garden.unit/px 10)
                             (garden.unit/em 2)
                             "--test-var"))
        "With Garden Units")
  (t/is (= "calc(10 + (50 - 10) * var(--test-var))"
           (fcss/interpolate 10
                             50
                             "--test-var"))
        "With any values"))
