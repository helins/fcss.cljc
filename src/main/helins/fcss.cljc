(ns helins.fcss

  ""

  (:require #?(:clj [clojure.edn])
            #?(:clj [clojure.string])
                    [garden.color]
                    [garden.compiler]
                    [garden.core           :as garden]
                    [garden.stylesheet]
                    [garden.util]
                    [helins.medium         :as medium]
            #?(:clj [helins.medium.co-load :as medium.co-load])
            #?(:clj [helins.fcss.compiler  :as fcss.compiler])
            #?(:clj [taoensso.timbre       :as log]))
  #?(:cljs (:require-macros [helins.fcss :refer [defclass
                                                 defdata
                                                 defid
                                                 defname
                                                 defvar
                                                 inspect*
                                                 namespaced-string*]]))
  #?(:clj (:import java.io.File
                   java.nio.file.Files
                   garden.color.CSSColor
                   garden.types.CSSUnit)))


(declare ^:private -prepare-rule+
                   templ)


;;;;;;;;;;


(medium/when-target* [:cljs/dev]
  
  (defonce ^:private -*state

    ;;

    (atom {:def-cycle {}
           :link+     {}})))


;;;;;;;;;;


#?(:clj (defn- -assoc-docstring

  ;;

  [sym docstring]

  (cond->
    sym
    docstring
    (vary-meta assoc
               :doc
               docstring))))


#?(:clj (defn- -docstring

  ""

  [arg+]

  (let [arg-first (first arg+)]
    (when (string? arg-first)
      arg-first))))



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
     (if fcss.compiler/dev?
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
                    placeholder->templatable))))))



(defn- -str-property

  ;;

  [property]
  
  (if (keyword? property)
    (name property)
    (str property)))



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
              (if fcss.compiler/dev?
                (sorted-map-by (fn [k-1 k-2]
                                 (compare (-str-property k-1)
                                          (-str-property k-2))))
                {})
             decl+)))




#?(:clj (defn- -prepare-anim

  ""

  [at-rule]

  (update-in at-rule
             [:value
              :frames]
             (fn [frame+]
               (mapv (fn [[step decl+]]
                       [step
                        (-templ-decl+ decl+)])
                     frame+)))))



#?(:clj (defn- -prepare-at-generic

  ""

  [var-rul at-rule]

  ;; For some reason, Garden needs rules to remain in a list.
  ;; A vector will result in the declarations not being rendered.

  (update-in at-rule
             [:value
              :rules]
             (fn [rule+]
               (map identity
                    (-prepare-rule+ var-rul
                                    rule+))))))



#?(:clj (defn- -prepare-at-rule

  ;;

  [var-rul {:as   at-rule
            :keys [identifier]}]

  (case identifier
    :keyframes (-prepare-anim at-rule)
    :media     (-prepare-at-generic var-rul
                                    at-rule)
    :feature   (-prepare-at-generic var-rul
                                    at-rule)
    :else      at-rule)))



#?(:clj (defn- -prepare-vector-rule

  ;;

  [rule]

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
         (-templ-decl+ decl+)]))))




#?(:clj (defn- -prepare-rule+

  ""


  ([var-rul rule+]

   (-prepare-rule+ var-rul
                   rule+
                   []))


  ([var-rul rule+ acc]

   (reduce (fn [acc-2 rul]
             (if (seq? rul)
               (-prepare-rule+ var-rul
                               rul
                               acc-2)
               (conj acc-2
                     (cond
                       (vector? rul)              (-prepare-vector-rule rul)
                       (garden.util/at-rule? rul) (-prepare-at-rule var-rul
                                                                    rul)
                       :else                      (throw (ex-info "CSS rule format not supported"
                                                                  {:fcss.error/namespace (ns-name *ns*)
                                                                   :fcss.error/rule      rul
                                                                   :fcss.error/type      :rule-format
                                                                   :fcss.error/var       var-rul}))))))
           acc
           rule+))))



(medium/when-target* [:cljs/dev]

  (defn ^:no-doc -ensure-link-node

    ;; Storing DOM does not work as they are somewhat recreated during live reloading.

    [sym-ns sym-var]

    (let [css-id      (str "fcss_dev__"
                           sym-ns
                           "__"
                           sym-var)
          [state-old
           state-new] (swap-vals! -*state
                                  (fn [state]
                                    (-> state
                                        (update-in [:def-cycle
                                                    sym-ns]
                                                   (fnil conj
                                                         #{})
                                                   sym-var)
                                        (assoc-in [:link+
                                                   sym-ns
                                                   sym-var]
                                                  css-id))))]
      (when-not (js/document.getElementById css-id)
        (let [dom-element (js/document.createElement "link")]
          (set! (.-className dom-element)
                "fcss_dev_link")
          (set! (.-href dom-element)
                (str "fcss/"
                     sym-ns
                     "/"
                     sym-var
                     ".css"))
          (set! (.-id dom-element)
                css-id)
          (set! (.-rel dom-element)
                "stylesheet")
          (.appendChild js/document.head
                        dom-element))))))



#?(:clj (defn ^:no-doc -add-rule+!

  ;;

  [var-rul rule-new+]

  (swap! fcss.compiler/*rule+
         (let [docstring   (-> var-rul
                               meta
                               :doc)
               rule-new-2+ (-prepare-rule+ var-rul
                                           rule-new+)
               sym         (symbol var-rul)
               nspace      (symbol (namespace sym))
               nme         (symbol (name sym))]
           (if fcss.compiler/dev?
             (fn [state]
               (let [rule-old+ (get-in state
                                       [nspace
                                        nme])]
                 (cond->
                   (vary-meta state
                              update-in
                              [:def-cycle
                               nspace]
                              (fnil conj
                                    #{})
                              nme)
                   (or (not= rule-new-2+
                             rule-old+)
                       (not= docstring
                             (-> rule-old+
                                 meta
                                 :fcss/docstring)))
                   (assoc-in [nspace
                              nme]
                             (with-meta rule-new-2+
                                        {:fcss.co-load/compile-cycle (medium.co-load/compile-cycle)
                                         :fcss/docstring             docstring})))))
             #(assoc %
                     sym
                     rule-new+))))))



#?(:clj (defn- -rul

  ;;

  [sym target form-def rule+]

  (if rule+
    (case target
      :cljs/dev (do
                  (fcss.compiler/compile-dev sym)
                  `(do
                     ~form-def
                     (-ensure-link-node (quote ~(ns-name *ns*))
                                        (quote ~sym))))
      :clojure  `(-add-rule+! ~form-def
                              ~(vec rule+))
      nil)
    form-def)))






(defmacro defrul

  ""

  {:arglists '([sym docstring? & rule+])}

  [sym & arg+]

  (let [target (medium/target &env)]
    (when-not (identical? target
                          :cljs/release)
      (let [docstring (-docstring arg+)]
        (-rul sym
              target
              `(def ~(-assoc-docstring sym
                                       docstring)
                    (quote ~(symbol (str *ns*)
                                    (name sym))))
              (cond->
                arg+
                docstring
                rest))))))



#?(:clj (defn- -defdualname

  ;;

  [env sym arg+ f-raw f-templated]

  (let [docstring (-docstring arg+)
        [option+
         rule+]   (loop [arg-2+  (cond->
                                   arg+
                                   docstring
                                   rest)
                         option+ nil]
                    (if (seq arg-2+)
                      (let [x (first arg-2+)]
                        (if (keyword? x)
                          (recur (drop 2
                                       arg-2+)
                                 (assoc option+
                                        x
                                        (second arg-2+)))
                          [option+
                           (not-empty (vec arg-2+))]))
                      [option+
                       nil]))

        raw       (f-raw (namespaced-string sym))
        target    (medium/target env)]
    (-rul sym
          target
          `(def ~(-assoc-docstring sym
                                   docstring)
                ~(case target
                   :cljs/dev    `(let [raw# ~raw]
                                   (.set -registry
                                         raw#
                                         ~(f-templated raw
                                                       option+))
                                   raw#)
                   :cljs/release raw
                   :clojure      `(->DualName ~raw
                                              ~(f-templated raw
                                                            option+))))
          rule+))))



(defmacro defclass

  ""

  {:arglists '([sym docstring?])}

  [sym & arg+]

  (-defdualname &env
                sym
                arg+
                identity
                (fn [raw _option+]
                  (str "."
                       raw))))



(defmacro defdata

  ""

  {:arglists '([sym docstring? rule+])}

  [sym & arg+]

  (let [docstring (-docstring arg+)]
    (-rul sym
          (medium/target &env)
          `(def ~(-assoc-docstring sym
                                   docstring)

             ~(str "data-"
                   (namespaced-string sym)))
          (cond->
            arg+
            docstring
            rest))))



(defmacro defid

  ""

  {:arglists '([sym docstring?])}

  [sym & arg+]

  (-defdualname &env
                sym
                arg+
                identity
                (fn [raw _option+]
                  (str "#"
                       raw))))



(defmacro defname

  ""

  {:arglists '([sym docstring?])}

  [sym & [docstring]]

  `(def ~(-assoc-docstring sym
                           docstring)

     ~(namespaced-string sym)))



(defmacro defvar

  ""

  {:arglists '([sym docstring? & option+])}

  [sym & arg+]

  (let [docstring (-docstring arg+)]
    (-defdualname &env
                  sym
                  arg+
                  (fn [namespaced-string]
                    (str "--"
                         namespaced-string))
                  (fn [raw option+]
                    (let [{:keys [fallback]} option+]
                      (if fallback
                        `(templ "var($name, $fallback)"
                                {:fallback ~fallback
                                 :name     ~raw})
                        (format "var(%s)"
                                raw)))))))



(defn- -flatten-on-seq

  ;;


  ([coll]

   (-flatten-on-seq coll
                    []))


  ([coll acc]

   (reduce (fn [acc-2 x]
             (if (seq? x)
               (-flatten-on-seq x
                                acc-2)
               (conj acc-2
                     x)))
           acc
           coll)))



(defn ^:no-doc -anim

  ""

  [css-name frame+]

  (apply garden.stylesheet/at-keyframes
         css-name
         (-flatten-on-seq frame+)))



(defmacro defanim

  ""

  {:arglists '([sym docstring? frame+])}

  [sym & arg+]

  (let [target (medium/target &env)]
    (when-not (identical? target
                          :cljs/release)
      (let [css-name  (namespaced-string sym)
            docstring (-docstring arg+)]
        `(do
           ~(-rul sym
                  (identical? (medium/target &env)
                              :cljs/dev)
                  docstring
                  `[(-anim ~css-name
                           ~(vec (cond->
                                   arg+
                                   docstring
                                   rest)))])
           (def ~(-assoc-docstring sym
                                   docstring)
             ~css-name))))))













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







(defmacro inspect*

  ""


  ([]

   (medium/not-cljs-release &env
                            &form)
   `(into (sorted-map)
          (for [[sym-ns#
                 hmap#]  (quote ~(deref fcss.compiler/*rule+))]
            [sym-ns#
             (into (sorted-map)
                   hmap#)])))


  ([sym]

   (medium/not-cljs-release &env
                            &form)
   (let [rule+   @fcss.compiler/*rule+
         var-rul (resolve sym)]
     (if var-rul
       (let [sym-resolved (symbol var-rul)]
         `(quote ~(get-in rule+
                          [(symbol (namespace sym-resolved))
                           (symbol (name sym-resolved))])))
       `(not-empty (into (sorted-map)
                         (quote ~(or (rule+ sym)
                                     (some->> (get (ns-aliases *ns*)
                                                   sym)
                                              ns-name
                                              rule+)))))))))











(medium/when-target* [:cljs/dev]
   
  (defn ^:no-doc -remove-link+

    ;;

    []

    (doseq [dom-element (vec (js/document.getElementsByClassName "fcss_dev_link"))]
      (.remove dom-element))
    nil))


; <!> Currently removed from API as it might confuse users.
;     If something is not right, it is best to simply empty the CLJS cache and restart the whole thing?
;
;
; #?(:clj (defn- ^:no-doc -clear
; 
;   ""
; 
;   [target]
; 
;   (reset! fcss.compiler/*rule+
;           nil)
;   (let [dir (File. fcss.compiler/dev-root)]
;     (when (.exists dir)
;       (doseq [^File dir-ns (.listFiles dir)]
;         (doseq [^File file-rul (.listFiles dir-ns)]
;           (Files/delete (.toPath file-rul)))
;         (Files/delete (.toPath dir-ns)))
;       (Files/delete (.toPath dir))))
;   (when (identical? target
;                     :cljs/dev)
;     `(-remove-link+))))
; 
; 
; 
; (defmacro clear*
; 
;   ""
; 
;   []
; 
;   (let [target (medium/target &env)]
;     (when (#{:cljs/dev
;              :clojure} target)
;       (-clear target))))
; 
; 
; 
; 
; (defmacro refresh*
; 
;   ""
; 
;   [path-css]
; 
;   (let [target (medium/target &env)]
;     (when (#{:cljs/dev
;              :clojure} target)
;       `(do
;          ~(-clear target)
;          ~(do
;             (reset! fcss.compiler/*rule+
;                     {})
;             (medium.co-load/clear!)
;             (medium/touch-recur path-css
;                                 medium/file-cljs?))))))



(medium/when-target* [:cljs/dev
                      :clojure]


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


;;;;;;;;;;


(defn- -def-cycle

  ;;

  [hmap def-cycle f-remove]

  (reduce-kv (fn [hmap def-ns def-sym+]
               (update hmap
                       def-ns
                       (fn [sym->x]
                         (reduce-kv (fn [sym->x-2 sym x]
                                      (if (contains? def-sym+
                                                     sym)
                                        sym->x-2
                                        (do
                                          (f-remove def-ns
                                                    sym
                                                    x)
                                          (dissoc sym->x-2
                                                  sym))))
                                    sym->x
                                    sym->x))))
             hmap
             def-cycle))



#?(:clj (defn- -compile-finish

  ;;

  [_param+]

  (swap! fcss.compiler/*rule+
         (fn [state]
           (-def-cycle (vary-meta state
                                  dissoc
                                  :def-cycle)
                       (-> state
                           meta
                           :def-cycle)
                       (fn [nspace sym _rule+]
                         (try
                           (-> (format "%s/%s/%s.css"
                                       fcss.compiler/dev-root
                                       nspace
                                       sym)
                               File.
                               .toPath
                               Files/delete)
                           (catch Throwable e
                             (log/error e
                                        (format "While deleting CSS dev file for stale rule: %s/%s"
                                                nspace
                                                sym))))))))))



#?(:clj (defn- -delete-dead-ns+

  ;;

  [{:medium.co-load/keys [unload+]}]

  (doseq [nspace unload+]
    (try
      (let [dir (File. (format "%s/%s"
                               fcss.compiler/dev-root
                               nspace))]
        (when (.exists dir)
          (log/info (format "Removing CSS dev files for unloaded namespace: %s"
                            nspace))
          (doseq [css-file (.listFiles dir)]
            (Files/delete (.toPath ^File css-file)))
          (Files/delete (.toPath dir))))
      (catch Throwable e
        (log/error e
                   (format "While deleting CSS dev files for unloaded namespace: %s"
                           nspace)))))))



#?(:clj (defn medium-plugin

  ""

  {:shadow.build/stages #{:compile-finish}}

  [{:as                param+
    :shadow.build/keys [stage]}]

  (let [f (case stage
            :compile-finish -compile-finish)]
    (f param+))
  (-delete-dead-ns+ param+)
  nil))



(medium/when-target* [:cljs/dev]

  (defn ^:dev/after-load ^:no-doc -after-load

    ;;

    []

    (let [v*remove (volatile! nil)]
      (swap! -*state
             (fn [{:as   state
                   :keys [def-cycle
                          link+]}]
               (vreset! v*remove
                        [])
               (assoc state
                      :def-cycle {}
                      :link+     (-def-cycle link+
                                             def-cycle
                                             (fn [_nspace _sym css-id]
                                               (some-> (js/document.getElementById css-id)
                                                       .remove)))))))))


;;;;;;;;;;


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
