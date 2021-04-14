;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.fcss

  "Defining CSS items: rules, classes, vars, ...

   Technically, this namespace implements most of the hot-reloading associated with
   those items.
  
   Also extra utilities such as writing interpolations in pure CSS.

   See README."

  (:require [clojure.string]
            #?(:clj [garden.color])
            #?(:clj [garden.compiler])
            #?(:clj [garden.stylesheet])
            #?(:clj [garden.util])
            #?(:clj [helins.coload        :as coload])
            [helins.medium                :as medium]
            #?(:clj [helins.fcss.compiler :as fcss.compiler])
            #?(:clj [taoensso.timbre      :as log]))
  #?(:cljs (:require-macros [helins.fcss :refer [defclass
                                                 defdata
                                                 defid
                                                 defname
                                                 defvar
                                                 inspect*
                                                 interpolate*
                                                 namespaced-string*
                                                 templ*]]))
  #?(:clj (:import java.io.File
                   java.nio.file.Files)))


#?(:clj (declare ^:private -prepare-rule+
                           templ))


;;;;;;;;;;


#?(:cljs (medium/when-target* [:cljs/dev]
  
  (defonce ^:private -*state

    ;; States meant for a Clojurescript dev environment.
    ;;
    ;; Keep tracks of symbols (CSS items) defined during a recompilation cycle
    ;; as well as what <link> nodes referencing CSS files exist.

    (atom {:def-cycle {}
           :link+     {}}))))


;;;;;;;;;; CSS properties


#?(:clj (def css-prop+

    "A set of non-exhaustive CSS properties.
    
     From [this list](https://www.w3.org/Style/CSS/all-properties.en.html].

     Used for alerting about possible typos."

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
      :pointer-events  ;; Added manually
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


;;;;;;;;;; Private helpers


#?(:clj (defn- -assoc-docstring

  ;; Adds a docstring to a symbol meant to be interned.

  [sym docstring]

  (cond->
    sym
    docstring
    (vary-meta assoc
               :doc
               docstring))))



#?(:clj (defn- -docstring

  ;; Returns the first argument from `arg+` if it is a docstring.

  [arg+]

  (let [arg-first (first arg+)]
    (when (string? arg-first)
      arg-first))))


;;;;;;;;;; Namespacing symbols to strings


#?(:clj (defn namespaced-string

  "Given a symbol, returns a string prefixing that symbol with the given namespace
   (or the current one).
  
   ```clojure
   (= \"foo__bar__ok\"
      (namespaced-string 'foo.bar
                         'ok))
   ```"

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

  "Macro for [[namespaced-string]].
  
   Meant for development, throws when used in CLJS release source."

  [sym]

  (medium/not-cljs-release*)
  (namespaced-string sym)))


;;;;;;;;;; Templating - Private


#?(:clj (defonce ^:no-doc -*dual-name+

     ;; Map of `raw name` to -> `templated name`, such as:
     ;;
     ;;   {"my-class" ".my-class"}
     ;;
     ;; See [[templ]].

     (atom {})))



#?(:clj (defn- -selector

  ;; A CSS selector is commonly a string such as ".my-class > p".
  ;;
  ;; In order to ease writing multi selectors, a vector of strings can be
  ;; provided and they will be joined with ",".

  [selector]

  (if (vector? selector)
    (clojure.string/join ",\n"
                         (map templ
                              selector))
    selector)))


;;;;;;;;;; Templating - Protocol, implementation, and API


#?(:clj (defprotocol ITemplate

  "Defines [[-templ]] which is a low-level function used by [[templ]]."

  (-templ [this]

    "The [[templ]] function uses this, one should read about it carefully.

     The common user will probably never have to implement this protocol.")))



#?(:clj (extend-protocol ITemplate

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
      (or (get @-*dual-name+
               string)
          string))))



#?(:clj (defn templ

    "Things defined using `def...` macros from this namespace usually have two representations.
   
     - One meant for code (eg. CSS class used in a component, such as \"my-class\")
     - One meant for CSS rules (eg. \".my-class\")

     This function serves to \"template\" that second representation.

     ```clojure
     (defclass my-class)

     (= (templ my-class)
        \".my-class\")
     ```

     This function relies on the protocol function [[-templ]] which is implemented by default for:

       - Things returned by `def...` functions from this namespace
       - Number
       - String
       - Object (pass through `str`)
       - `garden.color.CSSColor`
       - `garden.types.CSSUnit`

     For instance:

     ```clojure
     (= (templ (garden.units/px 50))
        \"50px\")
     ```

     Hence, it can be used to template strings and notably, CSS rule identifiers with placeholders.

     A placeholder is a substring prefixed with `$`. If there is only one, then `&` alone can be used
     such as: 

     ```clojure
     (= (templ \"$my-class > p\"
               {:my-class my-class})

        (templ \"$1 > p\"
               {1 my-class})

        (templ \"& > p\"
               my-class))
     ```

     It allows for writing complex CSS selectors.

     The templating itself is not particularly fast. It is meant for development and writing rules.
     In Clojurescript, usage is forbidden outside development and rules (compilation will throw)."

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
                                             (-templ %3))
                    template-2
                    placeholder->templatable))))))



#?(:clj (defmacro templ*

  "Meant to be used at the REPL from Clojure/script during development.
  
   See [[templ]]."

  ([templatable]

   (templ (eval templatable)))


  ([template placeholder->templatable]

   (templ (eval template)
          (eval placeholder->templatable)))))


;;;;;;;;;; Private - Templating CSS declarations in rule definitions (later)


#?(:clj (defn- -str-property

  ;; Stringify a CSS property.

  [property]
  
  (if (keyword? property)
    (name property)
    (str property))))



#?(:clj (defn- -templ-decl+

  ;; Used for templating CSS rules in definitions.
  ;;
  ;; Also inspects CSS properties and warns about a possible typo when they are
  ;; unknown.

  [var-def decl+]

  (reduce-kv (fn [decl-2+ property value]
               (assoc decl-2+
                      (if (keyword? property)
                        (do
                          (when-not (contains? css-prop+
                                               property)
                            (let [{:keys [file
                                          line]} (meta var-def)]
                              (log/warn (format "Maybe typo in CSS property '%s' for rule '%s' (%s, line %d)" 
                                                property
                                                (symbol var-def)
                                                file
                                                line))))
                            property)
                        (str property))
                      (if (vector? value)
                        (apply templ
                               value)
                        (templ value))))
              (if fcss.compiler/dev?
                (sorted-map-by (fn [k-1 k-2]
                                 (compare (-str-property k-1)
                                          (-str-property k-2))))
                {})
             decl+)))


;;;;;;;;;; Private - Preparing rules before prior to adding them to the global registry
;;
;; See [[-prepare-rule+]


#?(:clj (defn- -prepare-anim

  ;; Prepare a Garden animation rule.
  ;;
  ;; Used by [[-prepare-at-rule]].

  [var-def at-rule]

  (update-in at-rule
             [:value
              :frames]
             (fn [frame+]
               (mapv (fn [[step decl+]]
                       [step
                        (-templ-decl+ var-def
                                      decl+)])
                     frame+)))))



#?(:clj (defn- -prepare-at-generic

  ;; Prepares a common Garden "at-rule".
  ;;
  ;; Used by [[-prepare-at-rule]].

  [var-def at-rule]

  ;; For some reason, Garden needs rules to remain in a list (specifically).
  ;; A vector will result in the declarations not being rendered.

  (update-in at-rule
             [:value
              :rules]
             (fn [rule+]
               (list* (-prepare-rule+ var-def
                                      rule+))))))



#?(:clj (defn- -prepare-at-rule

  ;; Prepares a Garden "at-rule".
  ;;
  ;; It is a special record identifying things like CSS @keyframes.
  ;;
  ;; Used by [[-prepare-rule+]].

  [var-def {:as   at-rule
            :keys [identifier]}]

  (case identifier
    :keyframes (-prepare-anim var-def
                              at-rule)
    :media     (-prepare-at-generic var-def
                                    at-rule)
    :feature   (-prepare-at-generic var-def
                                    at-rule)
    :else      at-rule)))



#?(:clj (defn- -prepare-vector-rule

  ;; Prepares a "vector" rule, such as:
  ;;
  ;;   [".some-class"
  ;;    {:background :red}]
  ;;
  ;; Used by [[-prepare-rule+]].

  [var-def rule]

  (case (count rule)
    2 (let [[templatable
             decl+]      rule]
        [(templ templatable)
         (-templ-decl+ var-def
                       decl+)])
    3 (let [[template
             placeholder->templatable
             decl+]                   rule]
        [(templ template
                placeholder->templatable)
         (-templ-decl+ var-def
                       decl+)]))))




#?(:clj (defn- -prepare-rule+

  ;; Prepares a collection of rules.
  ;;
  ;; Preparing essentially serves to template what needs to be templated prior to adding those
  ;; rules to the global registry.


  ([var-def rule+]

   (-prepare-rule+ var-def
                   rule+
                   []))


  ([var-def rule+ acc]

   (reduce (fn [acc-2 rul]
             (if (seq? rul)
               (-prepare-rule+ var-def
                               rul
                               acc-2)
               (conj acc-2
                     (cond
                       (vector? rul)              (-prepare-vector-rule var-def
                                                                        rul)
                       (garden.util/at-rule? rul) (-prepare-at-rule var-def
                                                                    rul)
                       :else                      (throw (ex-info "CSS rule format not supported"
                                                                  {:fcss.error/namespace (ns-name *ns*)
                                                                   :fcss.error/rule      rul
                                                                   :fcss.error/type      :rule-format
                                                                   :fcss.error/var       var-def}))))))
           acc
           rule+))))


;;;;;;;;;; Private - Adding new rules


#?(:cljs (medium/when-target* [:cljs/dev]

  (defn ^:no-doc -ensure-link-node

    ;; Ensures each defined CSS symbol has a <link> node pointing to its CSS rules.
    ;;
    ;; (Storing <link>s does not work as they are somewhat recreated during live reloading.)

    [sym-ns sym-var]

    #?(:node nil
       :cljs (let [css-id (str "fcss_dev__"
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
                                 dom-element))))))))



#?(:clj (defn ^:no-doc -add-rule+!

  ;; Adds rules to the rule registry.
  ;;
  ;; On a per-symbol basis, check if new rules are different from possibly existing old ones
  ;; and only does an update if this is true.
  ;; This prevents unnecessary CSS recompilation and subsequent IO.

  [var-def rule-new+]

  (swap! fcss.compiler/*rule+
         (let [docstring   (-> var-def
                               meta
                               :doc)
               rule-new-2+ (-prepare-rule+ var-def
                                           rule-new+)
               sym         (symbol var-def)
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
                                        {:fcss/compile-cycle (or (coload/compile-cycle)
                                                                 0)
                                         :fcss/docstring     docstring})))))
             #(assoc-in %
                        [nspace
                         nme]
                        rule-new-2+))))))



#?(:clj (defn- -defrul

  ;; Entrypoint for defining rules.
  ;; Notably used by [[-defdualname]].
  ;;
  ;; Ensures rules are properly compiled into CSS files and imported in HTML via <link> nodes.

  [sym target form-def rule+]

  (if rule+
    (case target
      :cljs/dev     (do
                      (fcss.compiler/compile-dev sym)
                      `(do
                         ~form-def
                         (-ensure-link-node (quote ~(ns-name *ns*))
                                            (quote ~sym))))
      :cljs/release form-def
      :clojure      `(-add-rule+! ~form-def
                                  ~(vec rule+)))
    form-def)))


;;;;;;;;;; CSS definitions


#?(:clj (defmacro defany

  "Defines rules for anything.

   See README.


   ```clojure
   (defany misc-styles

     \"Various styles applying to HTML tags.\"

     [\"body\"
      {:background :green}]

     [\"p\"
      {:color :red}])
   ```"

  {:arglists '([sym docstring? & rule+])}

  [sym & arg+]

  (let [target (medium/target &env)]
    (when-not (identical? target
                          :cljs/release)
      (let [docstring (-docstring arg+)]
        (-defrul sym
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

  ;; Used by defining functions such as [[defclass]] or [[defid]].
  ;;
  ;; Checks if there is a docstring in arguments and processes it if found.
  ;;
  ;; `f-raw`       produces a name from a [[namespaced-string]]
  ;; `f-templated` produces a name from such a "raw" string.
  ;; See [[defclass]] for example.

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
    (-defrul sym
             target
             `(def ~(-assoc-docstring sym
                                      docstring)
                   ~(if (identical? target
                                    :clojure)
                      `(let [raw# ~raw]
                         (swap! -*dual-name+
                                assoc
                                raw#
                                ~(f-templated raw
                                              option+))
                         raw#)
                      raw))
             rule+))))



#?(:clj (defmacro defclass

  "Defines a CSS class.
  
   Rules can be provided and should, ideally, refer to that class.
  
   A special \"&\" identifier can be used for referencing it:
  
   ```clojure
   (defclass my-button

     \"Best button ever (optional docstring).\"

     [\"&\"
      {:background :pink
       :color      :orange}]
     [\"& > span\"
      {:font-size \"0.75rem\"}])
   ```"

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

  "Defines a CSS data property.

   See [[defclass]] for the meaning of `&`.
  
   ```clojure
   (defdata visible?

     [\"div[&=true]\"
      {:display :block]

     [\"div[&=false]\"
      {:display :none])
   ```"

  {:arglists '([sym docstring? rule+])}

  [sym & arg+]

  (let [docstring (-docstring arg+)]
    (-defrul sym
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

  "Defines a CSS id.

   CSS ids are not adviced in modern CSS.

   See [[defclass]] for similar example."

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

  "Low-level, seldom used.

   Uses [[namespaced-string]] to define such a string for a symbol,
   resulting in a unque name."

  {:arglists '([sym docstring?])}

  [sym & [docstring]]

  `(def ~(-assoc-docstring sym
                           docstring)

     ~(namespaced-string sym))))



#?(:clj (defmacro defvar

  "Defines a CSS variable.
  
   Available option is `:fallback` which provides a fallback value when
   the variable is not defined.

   ```clojure
   (defvar my-var
           :fallback (garden.units/px 42))
   ```"

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
                      `(str "var("
                            ~raw
                            ", "
                            (templ ~fallback)
                            ")")
                      (format "var(%s)"
                              raw)))))))


;;;;;;;;;; CSS definitions - Animations


#?(:clj (defn- -flatten-on-seq

  ;; A flatten a collection for each element that passes `seq?`.


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
           coll))))



#?(:clj (defn ^:no-doc -anim

  ""

  [css-name frame+]

  (apply garden.stylesheet/at-keyframes
         css-name
         (-flatten-on-seq frame+))))



#?(:clj (defmacro defanim

  "Defines an animation.
  
   See README."

  {:arglists '([sym docstring? frame+])}

  [sym & arg+]

  (let [target (medium/target &env)]
    (when-not (identical? target
                          :cljs/release)
      (let [css-name  (namespaced-string sym)
            docstring (-docstring arg+)]
        (-defrul sym
                 (medium/target &env)
                 `(def ~(-assoc-docstring sym
                                          docstring)
                       ~css-name)
                 `[(-anim ~css-name
                          ~(vec (cond->
                                  arg+
                                  docstring
                                  rest)))]))))))


;;;;;;;;;;


#?(:clj (defmacro inspect*

  "Meant for the REPL.
  
   Either from Clojure from Clojurescript, retrieves one or several defined CSS rules.
  
   No arguments will return the whole map of `namespace` -> (`name` -> `rules`).
  
   A qualified symbol returns the rules for that symbol, an unqualified one is considered to be
   a namespaced and returns a map of `name` -> `rules`"


  ([]

   (medium/not-cljs-release*)
   `(into (sorted-map)
          (for [[sym-ns#
                 hmap#]  (quote ~(deref fcss.compiler/*rule+))]
            [sym-ns#
             (into (sorted-map)
                   hmap#)])))


  ([sym]

   (medium/not-cljs-release*)
   (let [rule+   @fcss.compiler/*rule+
         var-def (resolve sym)]
     (if var-def
       (let [sym-resolved (symbol var-def)]
         `(quote ~(get-in rule+
                          [(symbol (namespace sym-resolved))
                           (symbol (name sym-resolved))])))
       `(not-empty (into (sorted-map)
                         (quote ~(or (rule+ sym)
                                     (some->> (get (ns-aliases *ns*)
                                                   sym)
                                              ns-name
                                              rule+))))))))))


;;;;;;;;;; CSS Interpolation and working


#?(:clj (defprotocol IInterpolate
  
  "Defines only one function types can implement for doing CSS interpolation."
  
  (interpolate [from to css-var]
  
    "CSS interpolation is about generating a string that leverages the CSS `calc` function for computing
     a transition between two values, such as two Garden colors.
   
     That transition depends on a CSS variable (either a string or the result of [[defvar]]), which have
     to be a value between 0 and 1 acting as a percentage.

     For example, two numbers:

     ```clojure
     (= (interpolate 0
                     255
                     \"--my-css-var\")

        \"calc(0 + ((255 - 0) * --my-css-var))\")
     ```

     More complex, two Garden colors:

     ```clojure
     (defvar some-percentage)

     (interpolate (garden.color/rgb 100 123 125)
                  (garden.color/hsl 278 0.2 0.3)
                  some-percentage)
     ```

     Already implemented for:

     - Numbers
     - Strings of your choice, are not processed
     - `garden.color.CSSColor`
     - `garden.color.CSSUnit`
     - Any object implementing [[ITemplate]]")))
  
  

#?(:clj (defn- -default-interpolate
  
  ;; Interpolation scheme used by several types.
  
  [from to css-var]
  
  (templ "calc($from + (($to - $from) * $var))"
         {:from from
          :to   to
          :var  css-var})))
  

  
#?(:clj (extend-protocol IInterpolate
  
    
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
                                  css-var))])))



#?(:clj (defmacro interpolate*

  ""

  [from to css-var]

  (eval `(interpolate ~from
                      ~to
                      ~css-var))))

;;;;;;;;;; Working with CSS variables


#?(:clj (defn fallback

  "Generates a string providing a fallback value for a var (which is  either a string
   or the result of [[defvar]]).
  
   ```clojure
   (defvar foo)

   (= (fallback \"--foo\"
                42)
      \"var(--foo, 42)\")
   ```"

  [css-var fallback-value]

  (format "var(%s, %s)"
          css-var
          (templ fallback-value))))


;;;;;;;;;; Plugin for Medium hook - Private


(defn- -def-cycle

  ;; Process a definition cycle (ie. a map of `namespace` -> `set of symbols` defined
  ;; during the current compilation cycle).
  ;;
  ;; `f-remove` is a side-effect producing function applied to all previously defined symbols
  ;; not defined in the current cycle (meaning they have been removed).
  ;;
  ;; See [[-compile-finish]] and [[-after-load]].

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

  ;; Used by [[coload]].
  ;;
  ;; Processes Shadow-CLJS `:compile-finish` compilation stage.
  ;;
  ;; This deletes CSS files for symbols that are not defined anymore.

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


;;;;;;;;;; Plugin for Medium hook - Public


#?(:clj (defn coload

  "Plugin for Coload hook.
  
   See README."

  {:shadow.build/stages #{:compile-finish}}

  [{:as                param+
    :coload/keys       [unload+]
    :shadow.build/keys [stage]}]

  (let [f (case stage
            :compile-finish -compile-finish)]
    (f param+))
  ;;
  ;; Deleting namespaces that are unloaded and never reloaded (ie. deleted).
  ;;
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
                           nspace)))))
  nil))


;;;;;;;;;; Deleting ununsed <link> nodes during dev


#?(:cljs (medium/when-target* :cljs/dev

  (defn ^:dev/after-load ^:no-doc -after-load

    ;; Lifecycle hook for Shadow-CLJS which deletes <link> nodes referencing removed CSS files.
    ;;
    ;; Different from a build hook, this is executed in CLJS

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
