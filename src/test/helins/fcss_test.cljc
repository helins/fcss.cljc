(ns helins.fcss-test

  ""

  (:require [clojure.string]
            [clojure.test  :as t]
            [garden.color]
            [garden.units  :as garden.unit]
            [helins.fcss   :as fcss]))


;;;;;;;;;;


(fcss/defclass css-class)

(fcss/defid css-id)

(fcss/defvar css-var)



(t/deftest color->hex

  (t/is (= "#000000ff"
           (fcss/color->hex (garden.color/hsla 0 0 0 1)))))


(t/deftest templ

  (t/is (= (str \.
                css-class)
           (fcss/templ css-class))
        "CSS class")

  (t/is (= (str \#
                css-id)
           (fcss/templ css-id))
        "CSS id")

  (t/is (= (str "var(" css-var ")")
           (fcss/templ css-var))
        "CSS var")

  (t/is (= "hsla(0, 0%, 0%, 1)"
           (fcss/templ (garden.color/hsla 0 0 0 1)))
        "Garden Color")

  (t/is (= "42px"
           (fcss/templ (garden.unit/px 42)))
        "Garden Unit")

  (t/is (= "42"
           (fcss/templ 42))
        "Number")

  (t/is (= ".class"
           (fcss/templ ".class"))
        "String")

  (t/is (= (str \. css-class ":hover")
           (fcss/templ "&:hover"
                       css-class))
        "With the special & placeholder")

  (t/is (= (str ".class:hover "
                \.
                css-class
                \#
                css-id)
           (fcss/templ "$1:hover $2$3"
                       {1  ".class"
                        :2 css-class
                        :3 css-id}))
        "Variadic"))





(t/deftest interpolate

  (t/is (= (clojure.string/replace "rgb( calc(0 + (100 - 0) * --test-var),
                                         calc(0 + (110 - 0) * --test-var),
                                         calc(0 + (120 - 0) * --test-var))"
                                   #"\s+"
                                   " ")
           (clojure.string/replace (fcss/interpolate (garden.color/hsl 0 0 0)
                                                     (garden.color/rgb 100 110 120)
                                                     "--test-var")
                                   #"\s+"
                                   " "))
        "With Garden Colors")
  (t/is (= (clojure.string/replace "rgba( calc(0 + (100 - 0) * --test-var),
                                          calc(0 + (110 - 0) * --test-var),
                                          calc(0 + (120 - 0) * --test-var),
                                          calc(1 + (0.5 - 1) * --test-var))" 
                                   #"\s+"
                                   " ")
           (clojure.string/replace (fcss/interpolate (garden.color/hsl 0 0 0)
                                                     (garden.color/rgba 100 110 120 0.5)
                                                     "--test-var")
                                   #"\s+"
                                   " "))
        "With alpha Garden Colors")
  (t/is (= "calc(10px + (2em - 10px) * --test-var)"
           (fcss/interpolate (garden.unit/px 10)
                             (garden.unit/em 2)
                             "--test-var"))
        "With Garden Units")
  (t/is (= "calc(10 + (50 - 10) * --test-var)"
           (fcss/interpolate 10
                             50
                             "--test-var"))
        "With any values"))
