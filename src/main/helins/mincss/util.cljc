(ns helins.mincss.util

  ""

  #?(:cljs (:require-macros [helins.mincss])))


;;;;;;;;;;


(defmacro advanced?

  ;;

  []

  (= (System/getProperty "helins.mincss.advanced")
     "true"))
