(ns helins.mincss.dev

  ""

  {:author "Adam Helinski"}

  (:require [garden.core   :as garden]
            [helins.mincss :as mincss]))


;;;;;;;;;;


(mincss/defvar bg)



(def rule+

  [["body"
    {:background (mincss/sub "var( $$ )"
                             {:$$ bg})
     bg          'blue}]])



(defn on-load

  ""

  []

  (mincss/global-style! (garden/css rule+)))



(comment

  (mincss/defclass foo)

  )
