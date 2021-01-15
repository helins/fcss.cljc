(ns helins.mincss

  ""

  (:require [clojure.string]
            [garden.core                    :as garden]
            [garden.types                   :as garden.type]
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











(defn anim

  ""

  [string-name frame+]

  (garden.type/->CSSAtRule :keyframes
                           {:frames     frame+
                            :identifier string-name}))









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



(defmacro defname

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
      (set! (.-id element)
            "helins-mincss--stylesheet")
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




(defn on-load!

  ""

  [rule+]

  (when goog.DEBUG
    (global-style! (garden/css rule+))))


))
