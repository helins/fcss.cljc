(ns helins.fcss

  ""

  (:require         [clojure.string]
                    [garden.color]
                    [garden.compiler]
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
                   java.nio.file.Files)))


(declare ^:private -prepare-rule+
                   templ)


;;;;;;;;;;


#?(:cljs (medium/when-target* [:cljs/dev]
  
  (defonce ^:private -*state

    ;;

    (atom {:def-cycle {}
           :link+     {}}))))


;;;;;;;;;;


(medium/when-target* [:cljs/dev
                      :clojure]

  (def css-prop+

    "From https://www.w3.org/Style/CSS/all-properties.en.html"

    #{:align-content
      :align-items
      :align-self
      :alignment-baseline
      :all
      :animation
      :animation-delay
      :animation-direction
      :animation-duration
      :animation-fill-mode
      :animation-iteration-count
      :animation-name
      :animation-play-state
      :animation-timing-function
      :appearance
      :aspect-ratio
      :azimuth
      :backface-visibility
      :background
      :background-attachment
      :background-blend-mode
      :background-clip
      :background-color
      :background-image
      :background-origin
      :background-position
      :background-repeat
      :background-size
      :baseline-shift
      :baseline-source
      :block-ellipsis
      :block-size
      :block-step
      :block-step-align
      :block-step-insert
      :block-step-round
      :block-step-size
      :bookmark-label
      :bookmark-level
      :bookmark-state
      :border
      :border-block
      :border-block-color
      :border-block-end
      :border-block-end-color
      :border-block-end-style
      :border-block-end-width
      :border-block-start
      :border-block-start-color
      :border-block-start-style
      :border-block-start-width
      :border-block-style
      :border-block-width
      :border-bottom
      :border-bottom-color
      :border-bottom-left-radius
      :border-bottom-right-radius
      :border-bottom-style
      :border-bottom-width
      :border-boundary
      :border-collapse
      :border-color
      :border-end-end-radius
      :border-end-start-radius
      :border-image
      :border-image-outset
      :border-image-repeat
      :border-image-slice
      :border-image-source
      :border-image-width
      :border-inline
      :border-inline-color
      :border-inline-end
      :border-inline-end-color
      :border-inline-end-style
      :border-inline-end-width
      :border-inline-start
      :border-inline-start-color
      :border-inline-start-style
      :border-inline-start-width
      :border-inline-style
      :border-inline-width
      :border-left
      :border-left-color
      :border-left-style
      :border-left-width
      :border-radius
      :border-right
      :border-right-color
      :border-right-style
      :border-right-width
      :border-spacing
      :border-start-end-radius
      :border-start-start-radius
      :border-style
      :border-top
      :border-top-color
      :border-top-left-radius
      :border-top-right-radius
      :border-top-style
      :border-top-width
      :border-width
      :bottom
      :box-decoration-break
      :box-shadow
      :box-sizing
      :box-snap
      :break-after
      :break-before
      :break-inside
      :caption-side
      :caret
      :caret-color
      :caret-shape
      :chains
      :clear
      :clip
      :clip-path
      :clip-rule
      :color
      :color-adjust
      :color-interpolation-filters
      :color-scheme
      :column-count
      :column-fill
      :column-gap
      :column-rule
      :column-rule-color
      :column-rule-style
      :column-rule-width
      :column-span
      :column-width
      :columns
      :contain
      :contain-intrinsic-size
      :content
      :content-visibility
      :continue
      :counter-increment
      :counter-reset
      :counter-set
      :cue
      :cue-after
      :cue-before
      :cursor
      :direction
      :display
      :dominant-baseline
      :elevation
      :empty-cells
      :fill
      :fill-break
      :fill-color
      :fill-image
      :fill-opacity
      :fill-origin
      :fill-position
      :fill-repeat
      :fill-rule
      :fill-size
      :filter
      :flex
      :flex-basis
      :flex-direction
      :flex-flow
      :flex-grow
      :flex-shrink
      :flex-wrap
      :float
      :float-defer
      :float-offset
      :float-reference
      :flood-color
      :flood-opacity
      :flow
      :flow-from
      :flow-into
      :font
      :font-family
      :font-feature-settings
      :font-kerning
      :font-language-override
      :font-optical-sizing
      :font-palette
      :font-size
      :font-size-adjust
      :font-stretch
      :font-style
      :font-synthesis
      :font-synthesis-small-caps
      :font-synthesis-style
      :font-synthesis-weight
      :font-variant
      :font-variant-alternates
      :font-variant-caps
      :font-variant-east-asian
      :font-variant-emoji
      :font-variant-ligatures
      :font-variant-numeric
      :font-variant-position
      :font-variation-settings
      :font-weight
      :footnote-display
      :footnote-policy
      :forced-color-adjust
      :gap
      :glyph-orientation-vertical
      :grid
      :grid-area
      :grid-auto-columns
      :grid-auto-flow
      :grid-auto-rows
      :grid-column
      :grid-column-end
      :grid-column-start
      :grid-row
      :grid-row-end
      :grid-row-start
      :grid-template
      :grid-template-areas
      :grid-template-columns
      :grid-template-rows
      :hanging-punctuation
      :height
      :hyphenate-character
      :hyphenate-limit-chars
      :hyphenate-limit-last
      :hyphenate-limit-lines
      :hyphenate-limit-zone
      :hyphens
      :image-orientation
      :image-rendering
      :image-resolution
      :initial-letter
      :initial-letter-align
      :initial-letter-wrap
      :inline-size
      :inline-sizing
      :inset
      :inset-block
      :inset-block-end
      :inset-block-start
      :inset-inline
      :inset-inline-end
      :inset-inline-start
      :isolation
      :justify-content
      :justify-items
      :justify-self
      :leading-trim
      :left
      :letter-spacing
      :lighting-color
      :line-break
      :line-clamp
      :line-grid
      :line-height
      :line-height-step
      :line-padding
      :line-snap
      :list-style
      :list-style-image
      :list-style-position
      :list-style-type
      :margin
      :margin-block
      :margin-block-end
      :margin-block-start
      :margin-bottom
      :margin-break
      :margin-inline
      :margin-inline-end
      :margin-inline-start
      :margin-left
      :margin-right
      :margin-top
      :margin-trim
      :marker
      :marker-end
      :marker-knockout-left
      :marker-knockout-right
      :marker-mid
      :marker-pattern
      :marker-segment
      :marker-side
      :marker-start
      :mask
      :mask-border
      :mask-border-mode
      :mask-border-outset
      :mask-border-repeat
      :mask-border-slice
      :mask-border-source
      :mask-border-width
      :mask-clip
      :mask-composite
      :mask-image
      :mask-mode
      :mask-origin
      :mask-position
      :mask-repeat
      :mask-size
      :mask-type
      :max-block-size
      :max-height
      :max-inline-size
      :max-lines
      :max-width
      :min-block-size
      :min-height
      :min-inline-size
      :min-width
      :mix-blend-mode
      :nav-down
      :nav-left
      :nav-right
      :nav-up
      :object-fit
      :object-position
      :offset
      :offset-anchor
      :offset-distance
      :offset-path
      :offset-position
      :offset-rotate
      :opacity
      :order
      :orphans
      :outline
      :outline-color
      :outline-offset
      :outline-style
      :outline-width
      :overflow
      :overflow-anchor
      :overflow-block
      :overflow-clip-margin
      :overflow-inline
      :overflow-wrap
      :overflow-x
      :overflow-y
      :overscroll-behavior
      :overscroll-behavior-block
      :overscroll-behavior-inline
      :overscroll-behavior-x
      :overscroll-behavior-y
      :padding
      :padding-block
      :padding-block-end
      :padding-block-start
      :padding-bottom
      :padding-inline
      :padding-inline-end
      :padding-inline-start
      :padding-left
      :padding-right
      :padding-top
      :page
      :page-break-after
      :page-break-before
      :page-break-inside
      :pause
      :pause-after
      :pause-before
      :perspective
      :perspective-origin
      :pitch
      :pitch-range
      :place-content
      :place-items
      :place-self
      :play-during
      :position
      :quotes
      :region-fragment
      :resize
      :rest
      :rest-after
      :rest-before
      :richness
      :right
      :rotate
      :row-gap
      :ruby-align
      :ruby-merge
      :ruby-overhang
      :ruby-position
      :running
      :scale
      :scroll-behavior
      :scroll-margin
      :scroll-margin-block
      :scroll-margin-block-end
      :scroll-margin-block-start
      :scroll-margin-bottom
      :scroll-margin-inline
      :scroll-margin-inline-end
      :scroll-margin-inline-start
      :scroll-margin-left
      :scroll-margin-right
      :scroll-margin-top
      :scroll-padding
      :scroll-padding-block
      :scroll-padding-block-end
      :scroll-padding-block-start
      :scroll-padding-bottom
      :scroll-padding-inline
      :scroll-padding-inline-end
      :scroll-padding-inline-start
      :scroll-padding-left
      :scroll-padding-right
      :scroll-padding-top
      :scroll-snap-align
      :scroll-snap-stop
      :scroll-snap-type
      :scrollbar-color
      :scrollbar-gutter
      :scrollbar-width
      :shape-image-threshold
      :shape-inside
      :shape-margin
      :shape-outside
      :spatial-navigation-action
      :spatial-navigation-contain
      :spatial-navigation-function
      :speak
      :speak-as
      :speak-header
      :speak-numeral
      :speak-punctuation
      :speech-rate
      :stress
      :string-set
      :stroke
      :stroke-align
      :stroke-alignment
      :stroke-break
      :stroke-color
      :stroke-dash-corner
      :stroke-dash-justify
      :stroke-dashadjust
      :stroke-dasharray
      :stroke-dashcorner
      :stroke-dashoffset
      :stroke-image
      :stroke-linecap
      :stroke-linejoin
      :stroke-miterlimit
      :stroke-opacity
      :stroke-origin
      :stroke-position
      :stroke-repeat
      :stroke-size
      :stroke-width
      :tab-size
      :table-layout
      :text-align
      :text-align-all
      :text-align-last
      :text-combine-upright
      :text-decoration
      :text-decoration-color
      :text-decoration-line
      :text-decoration-skip
      :text-decoration-skip-box
      :text-decoration-skip-ink
      :text-decoration-skip-inset
      :text-decoration-skip-self
      :text-decoration-skip-spaces
      :text-decoration-style
      :text-decoration-thickness
      :text-edge
      :text-emphasis
      :text-emphasis-color
      :text-emphasis-position
      :text-emphasis-skip
      :text-emphasis-style
      :text-group-align
      :text-indent
      :text-justify
      :text-orientation
      :text-overflow
      :text-shadow
      :text-space-collapse
      :text-space-trim
      :text-spacing
      :text-transform
      :text-underline-offset
      :text-underline-position
      :text-wrap
      :top
      :transform
      :transform-box
      :transform-origin
      :transform-style
      :transition
      :transition-delay
      :transition-duration
      :transition-property
      :transition-timing-function
      :translate
      :unicode-bidi
      :user-select
      :vertical-align
      :visibility
      :voice-balance
      :voice-duration
      :voice-family
      :voice-pitch
      :voice-range
      :voice-rate
      :voice-stress
      :voice-volume
      :volume
      :white-space
      :widows
      :width
      :will-change
      :word-boundary-detection
      :word-boundary-expansion
      :word-break
      :word-spacing
      :word-wrap
      :wrap-after
      :wrap-before
      :wrap-flow
      :wrap-inside
      :wrap-through
      :writing-mode
      :z-index}))







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



#?(:clj (defmacro namespaced-string*

  ""

  [sym]

  (medium/not-cljs-release &env
                           &form)
  (namespaced-string sym)))


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



#?(:clj (defn- -str-property

  ;;

  [property]
  
  (if (keyword? property)
    (name property)
    (str property))))



#?(:clj (defn- -templ-decl+

  ""

  [var-rul decl+]

  (reduce-kv (fn [decl-2+ property value]
               (assoc decl-2+
                      (if (keyword? property)
                        (do
                          (when-not (contains? css-prop+
                                               property)
                            (let [{:keys [file
                                          line]} (meta var-rul)]
                              (log/warn (format "Maybe typo in CSS property '%s' for rule '%s' (%s, line %d)" 
                                                property
                                                (symbol var-rul)
                                                file
                                                line))))
                            property)
                        (str property))
                      (cond->
                        value
                        (instance? DualName
                                   value)
                        templ)))
              (if fcss.compiler/dev?
                (sorted-map-by (fn [k-1 k-2]
                                 (compare (-str-property k-1)
                                          (-str-property k-2))))
                {})
             decl+)))




#?(:clj (defn- -prepare-anim

  ""

  [var-rul at-rule]

  (update-in at-rule
             [:value
              :frames]
             (fn [frame+]
               (mapv (fn [[step decl+]]
                       [step
                        (-templ-decl+ var-rul
                                      decl+)])
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
    :keyframes (-prepare-anim var-rul
                              at-rule)
    :media     (-prepare-at-generic var-rul
                                    at-rule)
    :feature   (-prepare-at-generic var-rul
                                    at-rule)
    :else      at-rule)))



#?(:clj (defn- -prepare-vector-rule

  ;;

  [var-rul rule]

  (case (count rule)
    2 (let [[templatable
             decl+]      rule]
        [(templ templatable)
         (-templ-decl+ var-rul
                       decl+)])
    3 (let [[template
             placeholder->templatable
             decl+]                   rule]
        [(templ template
                placeholder->templatable)
         (-templ-decl+ var-rul
                       decl+)]))))




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
                       (vector? rul)              (-prepare-vector-rule var-rul
                                                                        rul)
                       (garden.util/at-rule? rul) (-prepare-at-rule var-rul
                                                                    rul)
                       :else                      (throw (ex-info "CSS rule format not supported"
                                                                  {:fcss.error/namespace (ns-name *ns*)
                                                                   :fcss.error/rule      rul
                                                                   :fcss.error/type      :rule-format
                                                                   :fcss.error/var       var-rul}))))))
           acc
           rule+))))



#?(:cljs (medium/when-target* [:cljs/dev]

  (defn ^:no-doc -ensure-link-node

    ;; Storing DOM does not work as they are somewhat recreated during live reloading.

    [sym-ns sym-var]

    (let [css-id (str "fcss_dev__"
                      sym-ns
                      "__"
                      sym-var)]
      (swap! -*state
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
                             css-id))))
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
                        dom-element)))))))



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






#?(:clj (defmacro defrul

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
                rest)))))))



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



#?(:clj (defmacro defclass

  ""

  {:arglists '([sym docstring?])}

  [sym & arg+]

  (-defdualname &env
                sym
                arg+
                identity
                (fn [raw _option+]
                  (str "."
                       raw)))))



#?(:clj (defmacro defdata

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
            rest)))))



#?(:clj (defmacro defid

  ""

  {:arglists '([sym docstring?])}

  [sym & arg+]

  (-defdualname &env
                sym
                arg+
                identity
                (fn [raw _option+]
                  (str "#"
                       raw)))))



#?(:clj (defmacro defname

  ""

  {:arglists '([sym docstring?])}

  [sym & [docstring]]

  `(def ~(-assoc-docstring sym
                           docstring)

     ~(namespaced-string sym))))



#?(:clj (defmacro defvar

  ""

  {:arglists '([sym docstring? & option+])}

  [sym & arg+]

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



#?(:clj (defmacro defanim

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
             ~css-name)))))))













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







#?(:clj (defmacro inspect*

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
                                              rule+))))))))))




#?(:cljs (medium/when-target* [:cljs/dev]
   
  (defn ^:no-doc -remove-link+

    ;;

    []

    (doseq [dom-element (vec (js/document.getElementsByClassName "fcss_dev_link"))]
      (.remove dom-element))
    nil)))


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



#?(:cljs (medium/when-target* [:cljs/dev]

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
                                                       .remove))))))))))


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
