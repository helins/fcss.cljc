(ns hooks.helins.fcss

  ""

  {:author "Adam Helinski"}

  (:require [clj-kondo.hooks-api :as hook]))


;;;;;;;;;;


(defn defrul*

  ""

  [{:keys [node]}]

  (let [[_call
         sym
         & arg+]    (:children node)
        docstring   (first arg+)
        docstring-2 (when (hook/string-node? docstring)
                      docstring)
        ; rule+       (cond->
        ;               arg+
        ;               docstring-2
        ;               rest)
        ]
    {:node (hook/list-node (filter some?
                                   [(hook/token-node 'def)
                                    sym
                                    docstring-2
                                    ;;
                                    ;; The problem is that Garden units are defined in a non-standard way.
                                    ;; Hence, when used, Clj-kondo complains that they are not defined.
                                    ;;
                                    ;; (when rule+
                                    ;;   (hook/list-node (cons (hook/token-node 'do)
                                    ;;                         rule+)))
                                    ]))}))
