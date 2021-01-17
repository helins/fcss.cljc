(ns helins.fcss

  ""

  (:require [clojure.string]
            [garden.color]
            [garden.compiler]
            [garden.core                  :as garden]
            [garden.types                 :as garden.type]
            [garden.util]
            #?(:clj [helins.fcss.compiler :as fcss.compiler]))
  #?(:cljs (:require-macros [helins.fcss]))
  #?(:clj (:import garden.color.CSSColor
                   garden.types.CSSUnit)))


;;;;;;;;;;



(defn color->hex

  ""

  [color]

  (let [alpha (color :alpha)
        hex   (garden.color/as-hex color)]
    (cond->
      hex
      alpha
      (do
        (let [append (-> (Math/round (double (* alpha
                                                255)))
                         (garden.util/int->string 16))]
          (str hex
               (when (= (count append)
                        1)
                 "0")
               append))))))







(defprotocol ITemplate

 ; :extend-via-metadata  true

  ""

  (-templ [this]

    ""))



(extend-protocol ITemplate

  garden.color.CSSColor

    (-templ [color]
      (garden.compiler/render-css color))


  garden.types.CSSUnit

    (-templ [unit]
      (garden.compiler/render-css unit))


  #?(:clj  Number
     :cljs number)

    (-templ [n]
      (str n))


  #?(:clj   Object
     :cljs object)

    (-templ [o]
      (str o))


  #?(:clj   java.lang.String
     :cljs string)

    (-templ [string]
      string))






(defrecord Selector [raw
                     selector]

  ITemplate

    (-templ [_]
      selector)


  Object

    (toString [_]
      raw))




#?(:clj (do


(defn- -defselector

  ;;

  [sym docstring f-selector]

  (let [raw (fcss.compiler/magic (str *ns*)
                                 (name sym))]
    (concat `(def ~sym)
            (when docstring
              [docstring])
            [`(helins.fcss/->Selector ~raw
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

   (-templ templatable))


  ([template placeholder->templatable]

   (reduce-kv #(clojure.string/replace %1
                                       (name %2)
                                       (templ %3))
              (cond->>
                template
                (vector? template)
                (clojure.string/join ","))
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
                (fcss.compiler/magic (str *ns*)
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
            "helins-fcss--stylesheet")
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
                                                   "helins_fcss.css"
                                                   #js {"type" "text/css"}))))
  nil)




(defn on-load!

  ""

  [rule+]

  (when goog.DEBUG
    (global-style! (garden/css rule+))))


))
