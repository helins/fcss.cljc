(ns helins.fcss

  ""

  (:require [clojure.string]
            [garden.color]
            [garden.compiler]
            [garden.core                  :as garden]
            [garden.types                 :as garden.type]
            [garden.util]
            
            #?(:clj [clojure.tools.namespace.repl])

            #?(:clj [cljs.env])

            #?(:clj [helins.fcss.compiler :as fcss.compiler]))
  #?(:cljs (:require-macros [helins.fcss]))
  #?(:clj (:import java.io.File
                   garden.color.CSSColor
                   garden.types.CSSUnit)))


;;;;;;;;;;


#?(:clj (clojure.tools.namespace.repl/disable-reload!))






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





#?(:cljs 

(def ^:no-doc -registry

  ""

  (js/Map.)))





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


  #?(:clj  Object
     :cljs object)

    (-templ [o]
      (str o))


  #?(:clj  java.lang.String
     :cljs string)

    (-templ [string]
      #?(:clj  string
         :cljs (or (.get -registry
                         string)
                   string))))





(defrecord DualName [raw
                     selector]

  ITemplate

    (-templ [_]
      selector)


  Object

    (toString [_]
      raw))










#?(:clj (do


(defn- -dualname-body

  ;;

  [raw f-templated]

  (let [cljs-optimization (fcss.compiler/cljs-optimization)]
    (if (or fcss.compiler/*defrul?*
            (nil? cljs-optimization))
      `(helins.fcss/->DualName ~raw
                               ~(f-templated raw))
      (case cljs-optimization
        :dev     `(let [raw# ~raw]
                    (.set helins.fcss/-registry
                          raw#
                          ~(f-templated raw))
                    raw#)
        :release raw))))

    


(defn- -defdualname

  ;;

  [sym docstring f-raw f-templated]

  (let [raw (f-raw (fcss.compiler/magic (str *ns*)
                                        (clojure.string/replace (name sym)
                                                                #"\+$"
                                                                "s")))]
    
     (concat `(def ~(with-meta sym
                              {:foo :bar}))
            (when docstring
              [docstring])
            [(-dualname-body raw
                             f-templated)])))



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

  {:arglists '([sym docstring? & option+])}

  [sym & option+]

  (let [docstring  (first option+)
        docstring? (string? docstring)]
    (-defdualname sym
                  (when docstring?
                    docstring)
                  #(str "--"
                       %)
                  #(let [{:keys [fallback
                                 unit]}   (cond->
                                            option+
                                            docstring?
                                            rest)
                         string-1         (if fallback
                                            (format "var(%s, %s)"
                                                    %
                                                    fallback)
                                            (format "var(%s)"
                                                    %))]
                     (if unit
                       (format "calc(1%s * %s)"
                               (name unit)
                               string-1)
                       string-1)))))



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







(def path

  ""

  "./resources/public/fcss")







(defn rule

  ""

  ([templatable style]

   [(templ templatable)
    style])


  ([template placeholder->templatable style]

    [(templ template
            placeholder->templatable)
    style]))








#?(:cljs




(defn ^:no-doc -ensure-link-node

  ;;

  [css-id path]

  (let [node (js/document.getElementById css-id)]
    (when-not node
      (let [node-2 (js/document.createElement "link")]
        (set! (.-href node-2)
              path)
        (set! (.-id node-2)
              css-id)
        (set! (.-rel node-2)
              "stylesheet")
        (.appendChild js/document.head
                      node-2))))))


       



#?(:clj
   
(def ^:private -*rule+

  ;;

  (atom {})))



(defmacro rule-inspect

  ""

  ([]

   `(quote ~(deref -*rule+)))


  ([sym]

   `(quote ~(let [rule+   @-*rule+
                  var-sym (resolve &env
                                   sym)]
              (if var-sym
                (let [sym-2 @var-sym]
                  (get-in rule+
                          [(symbol (namespace sym-2))
                           (symbol (name sym-2))]))
                (or (get rule+
                         sym)
                    (get rule+
                         (some-> (get (ns-aliases *ns*)
                                      sym)
                                 str
                                 symbol))))))))



#?(:clj


(defn- -templ-decl+

  ""

  [env decl+]

  (reduce-kv #(assoc %1
                     (cond->
                       %2
                       (instance? DualName
                                  %2)
                       str)
                     (cond->
                       %3
                       (instance? DualName
                                  %3)
                       templ))
             {}
             decl+)))


(defmacro defrul

  ""

  ;; TODO. Ensures def to nil in advanced CLJS.

  {:arglists '([sym docstring? & rule+])}

  [sym & arg+]

  (let [cljs-optimization (fcss.compiler/cljs-optimization)
        clojure?          (nil? cljs-optimization)]
    (if (and (not fcss.compiler/*defrul?*)
               (or clojure?
                   (identical? cljs-optimization
                               :dev)))
      (do
        (binding [clojure.core/*e         nil
                  fcss.compiler/*defrul?* true]
          (when-not clojure?
            (clojure.tools.namespace.repl/refresh)
            (some-> clojure.core/*e
                    throw))
          (let [docstring  (first arg+)
                docstring? (string? docstring)
                rule+      (vec (cond->
                                   arg+
                                   docstring?
                                   rest))
                rule-2+    (try
                              (eval rule+)
                              (catch Throwable e
                                (throw (ex-info "Unable to eval CSS rules, are they written in propre CLJC?"
                                                {:helins.css/rule+ rule+}
                                                e))))
                rule-3+    (mapv (fn [rule]
                                   (case (count rule)
                                     2 (let [[templatable
                                              decl+]      rule]
                                         [(templ templatable)
                                          (-templ-decl+ &env
                                                        decl+)])
                                     3 (let [[template
                                              placeholder->templatable
                                              decl+]                   rule]
                                         [(templ template
                                                 placeholder->templatable)
                                          (-templ-decl+ &env
                                                        decl+)])))
                                 rule-2+)
                path-dir   (str path
                                "/"
                                *ns*)
                path-file  (str path-dir
                                "/"
                                (name sym)
                                ".css")
                css-id     (format "fcss__%s__%s"
                                   (str *ns*)
                                   (name sym))
                side-effet (if (fcss.compiler/compiling-cljs?)
                             `(helins.fcss/-ensure-link-node ~css-id
                                                             ~(format "./fcss/%s/%s.css"
                                                                      (str *ns*)
                                                                      (name sym)))
                             nil
                             )]
            (try
              (.mkdirs (File. path-dir))
              (catch Throwable e
                (throw (ex-info "Unable to create directory for dev CSS files"
                                {:helins.css.dev/path path-dir}
                                e))))
            (try
              (spit path-file
                    (cond->>
                      (garden/css rule-3+)
                      docstring?
                      (str "/* "
                           docstring
                           " */"
                           \newline
                           \newline)))
              (catch Throwable e
                (throw (ex-info "Unable to write CSS dev file"
                                {:helins.css.dev/path path-file}
                                e))))
            (swap! -*rule+
                   assoc-in
                   [(symbol (str *ns*))
                    sym]
                   rule-3+)
            `(do
               ~side-effet
               ~(concat `(def ~sym)
                        (when docstring?
                          [docstring])
                        [`(quote ~(symbol (str *ns*)
                                     (name sym)))])))))
      `(def ~sym (quote ~(symbol (str *ns*)
                                 (name sym)))))))




 



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
                            css-var))

  #?@(:cljs
       
       [string

          (interpolate [from to css-var]
            (-default-interpolate from
                                  to
                                  css-var))]))







(defn fallback

  ""

  [css-var fallback-value]

  (templ "var($var, $fallback)"
         {:fallback fallback-value
          :var      (str css-var)}))





#?(:clj


(defn -main

  ""

  [& arg]
  ))




