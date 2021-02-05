(ns helins.fcss

  ""

  (:require #?(:clj [clojure.edn])
            #?(:clj [clojure.string])
                    [garden.color]
                    [garden.compiler]
                    [garden.core           :as garden]
                    [garden.types          :as garden.type]
                    [garden.util]
                    [helins.medium         :as medium]
            #?(:clj [helins.medium.co-load :as medium.co-load])
            #?(:clj [helins.fcss.compiler  :as fcss.compiler]))
  #?(:cljs (:require-macros [helins.fcss :refer [clear*
                                                 defclass
                                                 defdata
                                                 defid
                                                 defname
                                                 defvar
                                                 namespaced-string*
                                                 refresh*]]))
  #?(:clj (:import java.io.File
                   garden.color.CSSColor
                   garden.types.CSSUnit)))


(declare templ)


;;;;;;;;;;


(def ^String path

  ""

  "./resources/public/fcss")


;;;;;;;;;;


#?(:clj (defn- -assoc-docstring

  ;;

  [sym docstring]

  (cond->
    sym
    docstring
    (vary-meta assoc
               :docstring
               docstring))))



#?(:clj (defn namespaced-string

  ""

  ([sym]

   (namespaced-string nil
                      sym))


  ([nmspace unqualified-sym]

   (let [string (str (clojure.string/replace (str (or nmspace
                                                      *ns*))
                                             "."
                                             "__")
                     "__"
                     (clojure.string/replace (str unqualified-sym)
                                             #"\+$"
                                             "s"))]
     (if (identical? medium/target-init
                     :cljs/dev)
       string
       (str fcss.compiler/tag-begin
            string
            fcss.compiler/tag-end))))))



(defmacro namespaced-string*

  ""

  [sym]

  (medium/not-cljs-release &env
                           &form)
  (namespaced-string sym))


;;;;;;;;;;


(medium/when-target* [:cljs/dev]

  (defonce ^:no-doc -registry
   
     (js/Map.)))



(medium/when-target* [:cljs/dev
                      :clojure]


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
                       templated]

    ITemplate

      (-templ [_]
        templated)


    Object

      (toString [_]
        raw)))



(defn- -selector

  ;;

  [selector]

  (if (vector? selector)
    (clojure.string/join ",\n"
                         (map templ
                              selector))
    selector))



(medium/when-target* [:cljs/dev
                      :clojure]

  (defn templ

    ""

    ([templatable]

     (-templ (-selector templatable)))


    ([template placeholder->templatable]

     (let [template-2 (-selector template)]
       (if (map? placeholder->templatable)
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
                    placeholder->templatable)
         (clojure.string/replace template-2
                                 "&"
                                 (templ placeholder->templatable)))))))



#?(:clj (defn- -templ-decl+

  ""

  [decl+]

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



#?(:clj (defn ^:no-doc -compile-rul

  ;;

  [sym docstring rule+]

  (let [rule-2+      (into []
                           (comp (mapcat #(if (every? vector?
                                                      %)
                                            %
                                            [%]))
                                 (filter some?)
                                 (map (fn [rule]
                                        (case (count rule)
                                          2 (let [[templatable
                                                   decl+]      rule]
                                              [(templ templatable)
                                               (-templ-decl+ decl+)])
                                          3 (let [[template
                                                   placeholder->templatable
                                                   decl+]                   rule]
                                              [(templ template
                                                      placeholder->templatable)
                                               (-templ-decl+ decl+)])))))
                           rule+)
        rule-cached+ (get-in @fcss.compiler/*rule+
                             [(ns-name *ns*)
                              sym])]
    (when (and (identical? medium/target-init
                           :cljs/dev)
               (or (not= docstring
                         (:fcss/docstring (meta rule-cached+)))
                   (not= rule-2+
                         rule-cached+)))
      (fcss.compiler/compile-dev path
                                 sym
                                 docstring
                                 rule-2+))
    (fcss.compiler/add-rule! sym
                             (with-meta rule-2+
                                        {:fcss/docstring docstring})))))



(medium/when-target* [:cljs/dev]

  (defn ^:no-doc -ensure-link-node

    ;;

    [css-id path]

    (let [node (js/document.getElementById css-id)]
      (when-not node
        (let [node-2 (js/document.createElement "link")]
          (set! (.-className node-2)
                "fcss_dev_link")
          (set! (.-href node-2)
                path)
          (set! (.-id node-2)
                css-id)
          (set! (.-rel node-2)
                "stylesheet")
          (.appendChild js/document.head
                        node-2))))))



#?(:clj (defn- -rul

  ;;

  [sym cljs-dev? docstring arg+]

  (when-some [rule+ (cond->
                      arg+
                      docstring
                      next)]
    (if cljs-dev?
      `(-ensure-link-node ~(format "fcss__%s__%s"
                                   (str *ns*)
                                   (name sym))
                          ~(format "./fcss/%s/%s.css"
                                   (str *ns*)
                                   (name sym)))
      (when-not (identical? medium/target-init
                            :cljs/release)
        `(-compile-rul '~sym
                        ~docstring
                        ~(vec rule+)))))))



(defmacro defrul

  ""

  {:arglists '([sym docstring? & rule+])}

  [sym & arg+]

  (let [target    (medium/target &env)
        cljs-dev? (identical? target
                              :cljs/dev)]
    (when (or cljs-dev?
              (identical? target
                          :clojure))
      (let [docstring  (first arg+)
            docstring? (string? docstring)]
        `(do
           ~(-rul sym
                  cljs-dev?
                  (when docstring?
                    docstring)
                  arg+)
           (def ~(-assoc-docstring sym
                                   docstring)

             (quote ~(symbol (str *ns*)
                             (name sym)))))))))


    
#?(:clj (defn- -defdualname

    ;;

    [env sym arg+ f-raw f-templated]

    (let [docstring   (first arg+)
          docstring-2 (when (string? docstring)
                        docstring)
          raw         (f-raw (namespaced-string sym))
          target      (medium/target env)
          cljs-dev?   (identical? target
                                  :cljs/dev)]
      `(do
         (def ~(-assoc-docstring sym
                                 docstring-2)

           ~(if cljs-dev?
             `(let [raw# ~raw]
                (.set -registry
                      raw#
                      ~(f-templated raw))
                raw#)
             (case target
               :cljs/release raw
               :clojure      `(->DualName ~raw
                                          ~(f-templated raw)))))
         ~(-rul sym
                cljs-dev?
                docstring-2
                arg+)))))




(defmacro ^{:arglists '([sym docstring?])}
          defclass

  ""

  [sym & arg+]

  (-defdualname &env
                sym
                arg+
                identity
                #(str \.
                      %)))



(defmacro ^{:arglists '([sym docstring?])}
          defdata

  ""

  [sym & [docstring]]

  `(def ~(-assoc-docstring sym
                           docstring)

     ~(str "data-"
           (namespaced-string sym))))



(defmacro ^{:arglists '([sym docstring?])}
          defid

  ""

  [sym & arg+]

  (-defdualname &env
                sym
                arg+
                identity
                #(str \#
                      %)))



(defmacro ^{:arglists '([sym docstring?])}
          defname

  ""

  [sym & [docstring]]

  `(def ~(-assoc-docstring sym
                           docstring)

     ~(namespaced-string sym)))



(defmacro ^{:arglists '([sym docstring?])}
          defvar

  ""

  {:arglists '([sym docstring? & option+])}

  [sym & option+]

  (let [docstring  (first option+)
        docstring? (string? docstring)]
    (-defdualname &env
                  sym
                  (when docstring?
                    docstring)
                  #(str "--"
                        %)
                  #(let [{:keys [fallback]} (cond->
                                              option+
                                              docstring?
                                              rest)]
                     (if fallback
                       `(templ "var($name, $fallback)"
                               {:fallback ~fallback
                                :name     ~%})
                       (format "var(%s)"
                               %))))))





















#?(:clj (defn rule

    ""

    ;; TODO. Remove

    ([templatable style]

     [(templ templatable)
      style])


    ([template placeholder->templatable style]

      [(templ template
              placeholder->templatable)
      style])))












(defmacro rule-inspect

  ""


  ([]

   (medium/not-cljs-release &env
                            &form)
   `(quote ~(deref fcss.compiler/*rule+)))


  ([sym]

   (medium/not-cljs-release &env
                            &form)
   `(quote ~(let [rule+   @fcss.compiler/*rule+
                  var-sym (resolve sym)]
              (if var-sym
                (let [sym-2 (symbol var-sym)]
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















(medium/when-target* [:cljs/dev]
   
  (defn ^:no-doc -remove-link+

    ;;

    []

    (doseq [dom-element (vec (js/document.getElementsByClassName "fcss_dev_link"))]
      (.remove dom-element))
    nil))



#?(:clj (defn- ^:no-doc -clear

  ""

  [target]

  (reset! fcss.compiler/*rule+
          nil)
  (let [dir (File. ^String path)]
    (doseq [dir-ns (.listFiles dir)]
      (doseq [file-rul (.listFiles dir-ns)]
        (.delete file-rul))
      (.delete dir-ns))
    (.delete dir))
  (when (identical? target
                    :cljs/dev)
    `(-remove-link+))))



(defmacro clear*

  ""

  []

  (let [target (medium/target &env)]
    (when (#{:cljs/dev
             :clojure} target)
      (-clear target))))




(defmacro refresh*

  ""

  [path-css]

  (let [target (medium/target &env)]
    (when (#{:cljs/dev
             :clojure} target)
      `(do
         ~(-clear target)
         ~(do
            (medium.co-load/reload-all!)
            (medium/touch-recur path-css
                                medium/file-cljs?))))))



(medium/when-target* [:cljs/dev
                      :clojure]

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
  
    (templ "calc($from + (($to - $from) * $var))"
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
            :var      (str css-var)})))


;;;;;;;;;


(defn color->hex

  ""

  ;; TODO. PR to Garden.

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
