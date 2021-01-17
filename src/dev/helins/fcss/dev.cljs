(ns helins.fcss.dev

  ""

  {:author "Adam Helinski"}

  (:require [garden.core :as garden]
            [helins.fcss :as fcss]))


;;;;;;;;;;


(fcss/defvar bg)



(def rule+

  [["body"
    {:background (fcss/templ "var( $$ )"
                             {:$$ bg})
     bg          'blue}]])



(defn on-load

  ""

  []

  (fcss/global-style! (garden/css rule+)))



(comment



  )
