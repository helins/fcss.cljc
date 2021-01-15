(ns helins.mincss-test

  ""

  (:require [clojure.set]
            [clojure.test  :as t]
            [helins.mincss :as mincss]))


;;;;;;;;;;



(t/deftest sel

  (t/is (= ".class"
           (mincss/sel "class"))
        "Arity 1")
  (t/is (= ".class-1:hover .class-2.class-3"
           (mincss/sel "$$1:hover $$2$$3"
                       {:$$1  "class-1"
                        '$$2  "class-2"
                        "$$3" "class-3"}))
        "Variadic"))


(t/deftest rule

  (t/is (= [".class"
            {:color 'red}]
           (mincss/rule "class"
                        {:color 'red}))
        "With a class name")
  (t/is (= [".class-1:hover .class-2"
            {:color 'red}]
           (mincss/rule "$1:hover $2"
                        {:$1 "class-1"
                         :$2 "class-2"}
                        {:color 'red}))
        "With placeholders"))
