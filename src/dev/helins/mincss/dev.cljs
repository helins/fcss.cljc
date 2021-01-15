(ns helins.mincss.dev

  ""

  {:author "Adam Helinski"}

  (:require [garden.core   :as garden]
            [helins.mincss :as mincss]))


;;;;;;;;;;


(def rule+

  [["body"
    {:background 'red}]])



(defn on-load

  ""

  []

  (mincss/global-style! (garden/css rule+)))



(comment


  )
