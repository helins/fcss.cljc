(ns helins.mincss

  ""

  (:require [clojure.string]
            [garden.core                    :as garden]
            [garden.types                   :as garden.type]
            #?(:clj [helins.mincss.compiler :as mincss.compiler]))
  #?(:cljs (:require-macros [helins.mincss])))


;;;;;;;;;;






(defprotocol ITemplate

 ; :extend-via-metadata  true

  ""

  (str-templ [this]

    ""))




(defrecord Selector [raw
                     selector]

  ITemplate

    (str-templ [_]
      selector)


  Object

    (toString [_]
      raw))




#?(:clj (do


(defn- -defselector

  ;;

  [sym docstring f-selector]

  (let [raw (mincss.compiler/magic (str *ns*)
                                   (name sym))]
    (concat `(def ~sym)
            (when docstring
              [docstring])
            [`(helins.mincss/->Selector ~raw
                                        ~(f-selector raw))])))



(defmacro defclass

  ""

  ([sym]

   `(defclass ~sym
              nil))


  ([sym docstring]

   (-defselector sym
                 docstring
                 #(str \.
                       %))))



(defmacro defid

  ""

  ([sym]

   `(defid ~sym
           nil))


  ([sym docstring]

   (-defselector sym
                 docstring
                 #(str \#
                       %))))

))





(defn templ

  ""

  ([templatable]

   (if (string? templatable)
     templatable
     (str-templ templatable)))


  ([template placeholder->templatable]

   (reduce-kv #(clojure.string/replace %1
                                       (name %2)
                                       (templ %3))
              template
              placeholder->templatable)))








(defn rule

  ""

  ([templatable style]

   [(templ templatable)
    style])

  ([template placeholder->templatable style]

   [(templ template
           placeholder->templatable)
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
          [(str prefix
                (mincss.compiler/magic (str *ns*)
                                       (name sym)))]))







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
