(ns helins.mincss-test

  ""

  (:require [clojure.test  :as t]
            [garden.color]
            [garden.units  :as garden.unit]
            [helins.mincss :as mincss]))


;;;;;;;;;;


(mincss/defclass sel-class)

(mincss/defid sel-id)



(t/deftest color->hex

  (t/is (= "#000000ff"
           (mincss/color->hex (garden.color/hsla 0 0 0 1)))))


(t/deftest templ

  (t/is (= ".class"
           (mincss/templ ".class"))
        "Arity 1 with a string")
  (t/is (= "hsla(0, 0%, 0%, 1)"
           (mincss/templ (garden.color/hsla 0 0 0 1)))
        "Arity 1 with a Garden Color")
  (t/is (= "42px"
           (mincss/templ (garden.unit/px 42)))
        "Arity 1 with a Garden Unit")
  (t/is (= "42"
           (mincss/templ 42))
        "Arity 1 with any object")
  (t/is (= (str \.
                sel-class)
           (mincss/templ sel-class))
        "Arity 1 with a Selector")
  (t/is (= (str ".class:hover "
                \.
                sel-class
                \#
                sel-id)
           (mincss/templ "$1:hover $2$3"
                         {:$1 ".class"
                          :$2 sel-class
                          :$3 sel-id}))
        "Variadic"))
