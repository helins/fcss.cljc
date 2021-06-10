;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.fcss.mode

  ""

  (:require [helins.medium :as medium])
  #?(:cljs (:require-macros [helins.fcss.mode :refer [current*]]))
  (:refer-clojure :exclude [switch!]))


;;;;;;;;;;


#?(:clj (def ^:private -mode

  ;;

  (let [env (System/getenv "FCSS_MODE")]
    (when env
      (case env
        ":dev"     :dev
        ":release" :release
        (do
          (println "Environment variable \"FCSS_MODE\" must be :dev or :release")
          (System/exit 42)))))))



#?(:clj (defn current

  ""

  [target]

  (or -mode
      (if (identical? target
                      :cljs/release)
        :release
        :dev))))



#?(:clj (defmacro current*

  ""

  []

  (current (medium/target &env))))



#?(:clj (defn switch!

  ""

  [new-mode]

  (assert (contains? #{nil
                       :dev
                       :release}
                     new-mode))
  (alter-var-root #'-mode
                  (constantly new-mode))))
