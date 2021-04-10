(ns helins.fcss.clj-kondo

  ""

  {:author "Adam Helinski"}

  (:require [clj-kondo.hooks-api :as hook]))


;;;;;;;;;;


(defn def*

  ""

  [{:keys [node]}]

  (let [[_call
         sym
         & arg+]    (:children node)
        docstring   (first arg+)
        docstring-2 (when (hook/string-node? docstring)
                      docstring)
        rule+       (cond->
                      arg+
                      docstring-2
                      rest)]
    {:node (hook/list-node (filter some?
                                   [(hook/token-node 'def)
                                    sym
                                    docstring-2
                                    (when rule+
                                      (hook/list-node (cons (hook/token-node 'do)
                                                            rule+)))]))}))
