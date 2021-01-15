(ns helins.mincss

  ""

  (:require [clojure.string]
            #?(:clj [helins.mincss.compiler :as mincss.compiler]))
  #?(:cljs (:require-macros [helins.mincss])))


;;;;;;;;;;











(defn- -to-string

  ""

  [x]
  (cond
    (keyword? x) (name x)
    (string? x)  x
    (symbol? x)  (str x)))




(defn sel

  ""

  ([class-name]

   (str \.
        class-name))


  ([selector placeholder->class-name]

   (reduce-kv #(clojure.string/replace %1
                                       (-to-string %2)
                                       (sel %3))
              selector
              placeholder->class-name)))



(defn sub

  ""

  [string placeholder->string]

  (reduce-kv #(clojure.string/replace %1
                                      (-to-string %2)
                                      %3)
             string
             placeholder->string))







(defn rule

  ""

  ([class-name style]

   [(sel class-name)
    style])

  ([selector placeholder->class-name style]

   [(sel selector
         placeholder->class-name)
    style]))



















#?(:clj (do


(defn- -defname

  ;;

  [sym docstring prefix]

  (concat `(def ~sym)
          (when docstring
            [docstring])
          (list (str prefix
                     (mincss.compiler/magic (str *ns*)
                                            (name sym))))))



(defmacro defclass

  ""

  ([sym]

   (-defname sym
             nil
             nil))


  ([sym docstring]

   (-defname sym
             docstring
             nil)))



(defmacro defvar

  ""

  ([sym]

   (-defname sym
             nil
             "--"))


  ([sym docstring]

   (-defname sym
             docstring
             "--")))


))















#?(:cljs (do


(defonce ^:private -d*element-stylesheet

  ;;

  (delay
    (let [element (js/document.createElement "link")
          v*url   (volatile! nil)]
      (set! (.-rel element)
            "stylesheet")
      (.appendChild js/document.head
                    element)
      element)))



(defonce ^:private -v*url

  ;;

  (volatile! nil))



(defn global-style! 

  ""

  [css-string]

  (some-> @-v*url
          js/URL.revokeObjectURL)
  (set! (.-href @-d*element-stylesheet)
        (vreset! -v*url
                 (js/URL.createObjectURL (js/File. [css-string]
                                                   "helins_mincss.css"
                                                   #js {"type" "text/css"}))))
  nil)





))
