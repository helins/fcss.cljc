;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.fcss.dev

  ""

  {:author "Adam Helinski"}

  (:require [helins.fcss         :as fcss]
            [helins.fcss.dev.css :as fcss.dev.css]
            [helins.fcss.mode    :as fcss.mode]))


;;;;;;;;;;


(defn main

  []

  (println :Classes
           fcss.dev.css/klass))


(comment



  )
