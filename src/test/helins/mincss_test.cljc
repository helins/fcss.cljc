(ns helins.mincss-test

  ""

  (:require [clojure.test  :as t]
            [helins.mincss :as mincss]))


;;;;;;;;;;


(mincss/defclass sel-class)

(mincss/defid sel-id)



(t/deftest templ

  (t/is (= ".class"
           (mincss/templ ".class"))
        "Arity 1 with a string")
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
