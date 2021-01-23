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






(defrecord DualName [raw
                     selector]

  ITemplate

    (-templ [_]
      selector)


  Object

    (toString [_]
      raw))





#?(:clj (do


(defn- -defdualname

  ;;

  [sym docstring f-raw f-selector]

  (let [raw (f-raw (fcss.compiler/magic (str *ns*)
                                        (clojure.string/replace (name sym)
                                                                #"\+$"
                                                                "s")))]
    (concat `(def ~sym)
            (when docstring
              [docstring])
            [`(helins.fcss/->DualName ~raw
                                      ~(f-selector raw))])))



(defmacro defclass

  ""

  ([sym]

   `(defclass ~sym
              nil))


  ([sym docstring]

   (-defdualname sym
                 docstring
                 identity
                 #(str \.
                       %))))



(defmacro defid

  ""

  ([sym]

   `(defid ~sym
           nil))


  ([sym docstring]

   (-defdualname sym
                 docstring
                 identity
                 #(str \#
                       %))))

))





(defn templ

  ""

  ([templatable]

   (-templ templatable))


  ([template placeholder->templatable]

   (let [template-2 (if (vector? template)
                      (clojure.string/join ","
                                           (map templ
                                                template))
                      template)]
     (if (clojure.string/includes? template-2
                                   "&")
       (clojure.string/replace template-2
                               "&"
                               (templ placeholder->templatable))
       (reduce-kv #(clojure.string/replace %1
                                           (cond
                                             (keyword? %2) (str \$
                                                                (name %2))
                                             (number? %2)  (str \$
                                                                %2)
                                             :else         (throw (ex-info "Placeholder must be keyword or number"
                                                                           {:placeholder %2
                                                                            :template    template})))
                                           (templ %3))
                  template-2
                  placeholder->templatable)))))








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






(defprotocol IInterpolate

  ""

  (interpolate [from this css-var]

    ""))




(defn- -default-interpolate

  ;;

  [from to css-var]

  (templ "calc($from + ($to - $from) * $var)"
         {:from from
          :to   to
          :var  css-var}))




(extend-protocol IInterpolate

  
  garden.color.CSSColor

    (interpolate [from to css-var]
      (when-not (instance? garden.color.CSSColor
                           to)
        (throw (ex-info "Cannot interpolate color to non-color"
                        {:from from
                         :to   to})))
      (let [alpha-1   (:alpha from)
            alpha-2   (:alpha to)
            rgb-1     (garden.color/as-rgb from)
            rgb-2     (garden.color/as-rgb to)
            calc+     (templ "calc($r-1 + ($r-2 - $r-1) * $var), calc($g-1 + ($g-2 - $g-1) * $var), calc($b-1 + ($b-2 - $b-1) * $var)"
                             {:b-1 (rgb-1 :blue)
                              :b-2 (rgb-2 :blue)
                              :r-1 (rgb-1 :red)
                              :r-2 (rgb-2 :red)
                              :g-1 (rgb-1 :green)
                              :g-2 (rgb-2 :green)
                              :var css-var})]
        (if (or alpha-1
                alpha-2)
          (templ "rgba( $calc+, calc($a-1 + ($a-2 - $a-1) * $var))"
                 {:a-1   (or alpha-1
                             1)
                  :a-2   (or alpha-2
                             1)
                  :calc+ calc+
                  :var   css-var})
          (str "rgb( " calc+ ")"))))


  garden.types.CSSUnit

    (interpolate [from to css-var]
      (when-not (instance? garden.types.CSSUnit
                           to)
        (throw (ex-info "Cannot interpolate unit to non-unit"
                        {:from from
                         :to   to})))
      (-default-interpolate from
                            to
                            css-var))

  #?@(:cljs
       
       [number

          (interpolate [from to css-var]
            (-default-interpolate from
                                  to
                                  css-var))])
          

  #?(:clj  Object
     :cljs object)

    (interpolate [from to css-var]
      (-default-interpolate from
                            to
                            css-var)))






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

   `(defvar ~sym
            nil))


  ([sym docstring]

   (-defdualname sym
                 docstring
                 #(str "--"
                       %)
                 #(format "var(%s)"
                          %))))





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
