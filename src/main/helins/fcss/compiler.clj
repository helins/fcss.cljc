;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at https://mozilla.org/MPL/2.0/.


(ns helins.fcss.compiler

  ""

  (:require [clojure.java.io]
            [clojure.pprint]
            [clojure.string]
            [garden.core      :as garden]
            [garden.compiler]
            [helins.coload    :as coload]
            [taoensso.timbre  :as log])
  (:import java.io.File))


;;;;;;;;;;


(def dev?

  "Is the environment set to dev mode?
  
   Attention, becomes altered to true when in release mode."

  true)



(def ^String dev-root

  "Path were dev CSS files are produced."

  "fcss/dev/fcss")


;;;;;;;;;; Miscellaneous


(defn css-var?

  "Is this a CSS variable?
  
   Returns true if string starts with \"--\"."

  [string]

  (clojure.string/starts-with? string
                               "--"))


;;;;;;;;;; Central registry for CSS rules


(defonce *rule+

  ;; Global rule registry.

  (atom {}))


;;;;;;;;;; Tagging strings and working with them


(def tag-begin

  "A tagged CSS item, such as a class, begins with this string."

  "<<<_FCSS_")



(def tag-end

  "A tagged CSS item, such as a class, ends with this string."

  "_FCSS_>>>")


;;;;; Regexes for finding different kinds of tagged strings


(let [base-pattern (str tag-begin
                  	    "\\S+?"
                  	    tag-end)]

  (def regex-tagged

    "Regex for a tagged CSS item (class or var)."

    (re-pattern (str "(?:--)?"
                     base-pattern)))


  (def regex-tagged-class

	"Regex for a tagged CSS class."

	(re-pattern base-pattern))


  (def regex-tagged-var

    "Regex for a tagged CSS var."

    (re-pattern (str "--"
                     base-pattern)))


  (def regex-tagged-dotted-class

    "Regex for a tagged CSS class in CSS notation (beginning with a dot)."
	
    (re-pattern (str "\\."
					 base-pattern))))



(defn detect-tag+

  "Given a string, detects tagged classes and variables.
  
   Returns a map containing `:class+` and `:var+`, pointing to sets containing those
   names."

  [string]

  (reduce (fn [acc tagged]
            (update acc
                    (if (css-var? tagged)
                      :var+
                      :class+)
                    conj
                    tagged))
          {:class+ #{}
           :var+   #{}}
          (re-seq regex-tagged
                  string)))


;;;;;;;;;; Compiling CSS for dev


(defn compile-dev

  ""

  
  ([unqualified-sym]

   (compile-dev nil
                unqualified-sym))


  ([nmspace unqualified-sym]

   (let [nmspace-2 (or nmspace
                       *ns*)
         path-dir  (format "%s/%s"
                           dev-root
                           nmspace-2)
         path-file (format "%s/%s.css"
                           path-dir
                           unqualified-sym)
         var-rul   (ns-resolve nmspace-2
                               unqualified-sym)]
     (if var-rul
       (let [sym-rul   (symbol var-rul)
             rule+     (get-in @*rule+
                               [(symbol (namespace sym-rul))
                                (symbol (name sym-rul))])]
         (when (= (-> rule+
                      meta
                      :fcss/compile-cycle)
                  (coload/compile-cycle))
           (try
             (.mkdirs (File. path-dir))
             (catch Throwable e
               (throw (ex-info "Unable to create directory for dev CSS files"
                               {:fcss/path path-dir
                                :fcss/sym  sym-rul}
                               e))))
           (try
             (spit path-file
                   (let [docstring (-> var-rul
                                       meta
                                       :doc)]
                     (cond->>
                       (garden/css rule+)
                       docstring
                       (str "/* "
                            docstring
                            " */"
                            \newline
                            \newline))))
             (catch Throwable e
               (throw (ex-info "Unable to write CSS dev file"
                               {:fcss/path path-file}
                               e))))))
       (log/error (format "Unable to resolve symbol '%s' in namespace '%s'"
                          unqualified-sym
                          (str nmspace-2)))))))


;;;;;;;;;; Compiling and optimizing CSS for release


(defn add-rule

  "Adds a CSS `rule` to the given `ctx`."

  [ctx rule]

  (update ctx
          :rule+
          conj
          rule))



(defn atomize-rule+

  "Atomizing a rule means, for a CSS class, splitting that rule into individual declarations and adding
   all that to a map of `single declaration` -> `set of CSS classes`.

   Atomization happens only for CSS selectors consisting of only a tagged CSS class and nothing else.

   Tagged classes that are not found in `detected-name+` are simply eliminated, resulting in dead CSS
   elimination.

   This function updates the given context with those keys:

   | Key | Is |
   |---|---|
   | :decl->class+ | A map of `single declaration` -> `set of CSS classes` |
   | :rule+ | Existing vector is emptied and then filled with rules not targetting CSS classes |
   | :rule-complex+ | Vector of rules with complex selectors where at least one CSS class is both tagged and detected |"

  ;; TODO. Keep multiple selectors as a vector when prepareing rules in core namespace?

  [{:as   ctx
    :keys [detected-name+
           rule+]}]

  (reduce (fn [ctx rule]
            (cond
              (vector? rule) (let [[str-selector+
                                    decl+ ]       rule]
                               ;;
                               ;; Selector with consisting of one tagged class.
                               ;;
                               (if-some [dotted-class-name (re-matches regex-tagged-dotted-class
                                                                       str-selector+)]
                                 (let [class-name (.substring ^String dotted-class-name
                                                              1)]
                                   (if (contains? detected-name+
                                                  class-name)
                                     (update ctx
                                             :decl->class+
                                             (fn [decl->class+]
                                               (reduce #(update %1
                                                                %2
                                                                (fnil conj
                                                                      #{})
                                                                class-name)
                                                       decl->class+
                                                       decl+)))
                                     ctx))
                                 ;;
                                 ;; Checking for a complex selector (containing at least one tagged and detected CSS class)
                                 ;; 
                                 ;;
                                 (if-some [class-name+ (not-empty (into #{}
                                                                        (re-seq regex-tagged-class
                                                                                str-selector+)))]
                                   (if (= (count (filter detected-name+
                                                         class-name+))
                                          (count class-name+))

                                     (update ctx
                                             :rule-complex+
                                             conj
                                             {:class-name+ class-name+
                                              :decl+       decl+
                                              :selector    str-selector+})
                                     ctx)
                                   (add-rule ctx
                                             rule))))
              :else          (add-rule ctx
                                       rule)))
          (assoc ctx
                 :decl->class+  {}
                 :rule+         []
                 :rule-complex+ [])
          rule+))



(defn group-decl+

  "Must be called after [[atomize-rule+]].
  
   Inverses `decl->class+` by assoc'ing to the given `ctx`, at `:class+->style`,
   a map of `set of CSS classes` -> `CSS style` (regrouped declarations).

   This computes groups of classes that shares exactly or partly the same style."

  [{:as   ctx
    :keys [decl->class+]}]

  (assoc ctx
         :class+->style
         (reduce-kv (fn [class+->style decl class+]
                      (update class+->style
                              class+
                              (fnil conj
                                    {})
                              decl))
                    {}
                    decl->class+)))



(defn munge-class+

  "Must be called after [[group-decl+]].

   Updates the `ctx` with:

   | Key | Is |
   |---|---|
   | :class->unique-munged | Map of `CSS class` -> `munged named referring only to that class` |
   | :original->munged+ | Map of `CSS class` -> `vector of munged names` |
   | :seed | Number used for munging that is being incremented as names are produced |"

  [{:as        ctx
    :keys      [class+->style]
    :fcss/keys [prefix]}]

  (reduce-kv (fn [ctx-2 class+ style]
               (let [seed-2       (inc (ctx-2 :seed))
                     munged-class (str prefix
                                       seed-2)
                     ctx-3        (-> ctx-2
                                      (update :original->munged+
                                              (fn [original->munged+]
                                                (reduce #(update %1
                                                                 %2
                                                                 (fnil conj
                                                                       [])
                                                                 munged-class)
                                                        original->munged+
                                                        class+)))
                                      (update :rule+
                                              conj
                                              [(str \.
                                                    munged-class)
                                               style])
                                      (assoc :seed
                                             seed-2))
                     unique-class     (when-not (next class+)
                                        (first class+))]
                 (cond->
                   ctx-3
                   unique-class
                   (update :class->unique-munged
                           assoc
                           unique-class
                           munged-class))))
             (-> ctx
                 (assoc :class->unique-munged {}
                        :original->munged+    {})
                 (update :seed
                         #(or %
                              0)))
             class+->style))



(defn- -process-complex

  ""

  [ctx class-name class-name-munged rule-complex+]

  (-> ctx
      (update :class->rule-complex+
              dissoc
              class-name)
      (update :rule+
              into
              (map (fn [[str-selector style]]
                     [(clojure.string/replace str-selector
                                              class-name
                                              class-name-munged)
                      style]))
              rule-complex+)))



(defn ensure-unique

  "Ensures that each CSS class has a munged named referring only to that class.
  
   See [[munge-class+]].
  
   Used by [[ensure-unique]]."

  [ctx class-name]

  (let [path-unique  [:class->unique-munged
                      class-name]]
    (if (get-in ctx
                path-unique)
      ctx
      (let [seed-2       (inc (ctx :seed))
            munged-class (str (ctx :fcss/prefix)
                              seed-2)]
        (-> ctx
            (assoc :seed
                   seed-2)
            (update-in [:original->munged+
                        class-name]
                       (fnil conj
                             [])
                       munged-class)
            (assoc-in path-unique
                      munged-class))))))



(defn rule-complex+

  "Must be called after [[munge-class+]].

   Processes complex CSS rules (described in [[atomize-rule+]]) and adds them
   to the `ctx` under `:rule+`.
  
   The `:rule-complex+` key is dissoc'ed."

  [{:as   ctx
    :keys [rule-complex+]}]

  (reduce (fn [ctx-2 {:keys [class-name+
                             decl+
                             selector]}]
            (let [ctx-3      (reduce ensure-unique
                                     ctx-2
                                     class-name+)
                  selector-2 (reduce (fn [selector-2 [class-name munged]]
                                       (clojure.string/replace selector-2
                                                               class-name
                                                               munged))
                                     selector
                                     (map (juxt identity
                                                (ctx-3 :class->unique-munged))
                                          class-name+))]
              (update ctx-3
                      :rule+
                      conj
                      [selector-2
                       decl+])))
          (dissoc ctx
                  :rule-complex+)
          rule-complex+))



(defn class-join-munged

  "Must be called after [[rule-complex+]].

   In `ctx`, under `:class->munged`, assoc'es a map of `CSS class` -> `string
   gathering all related munged names`."

  [{:as   ctx
    :keys [original->munged+]}]

  (assoc ctx
         :class->munged
         (into {}
               (for [[class-name munged+] original->munged+]
                 [class-name
                  (clojure.string/join " "
                                       munged+)]))))



(defn munge-var+

  "Akin to [[munge-class+]], munges the given CSS vars and assoc'es them
   in the `ctx` under `:var->munged`."

  [ctx var+]

  (let [{:fcss/keys [prefix]} ctx]
    (reduce (fn [ctx-2 tagged-var]
              (let [seed-2 (inc (ctx-2 :seed))]
                (-> ctx-2
                    (assoc :seed
                           seed-2)
                    (update :var->munged
                            assoc
                            tagged-var
                            (str "--"
                                 prefix
                                 seed-2)))))
            ctx
            var+)))



(defn compile-rule+

  "Must be called after [[class-join-munged]].
  
   After all that work, compiles all surviving rules to CSS and assoc'es them
   in `ctx` undex `:output`."

  [{:as   ctx
    :keys [rule+]}]

  (assoc ctx
         :output
         (garden/css rule+)))



(defn rename-in-output

  "Must be called after [[compile-rule+]], when surviving rules have been compiled to CSS.
  
   Previously detected CSS classes and vars are substituted for their munged names in `:output`."

  ;; TODO. Warn when a declaration refers to something that is not used in code (eg. CSS var not defined in advanced build).

  [{:as        ctx
    :keys      [output
                var->munged]
    :fcss/keys [prefix]}]

  (let [v*ctx    (volatile! ctx)
        output-2 (clojure.string/replace output
                                         regex-tagged
                                         (fn [magic-name]
                                           (if (css-var? magic-name)
                                             (or (get var->munged
                                                      magic-name)
                                                 (str magic-name
                                                      "__DEAD"))
                                             (let [path-munged [:class->munged
                                                                magic-name]]
                                               (get-in (vswap! v*ctx
                                                               (fn [ctx-2]
                                                                 (cond->
                                                                   ctx-2
                                                                   (not (get-in ctx-2
                                                                                path-munged))
                                                                   (do
                                                                     (let [seed-2 (inc (ctx-2 :seed))]
                                                                       (-> ctx-2
                                                                           (assoc :seed
                                                                                  seed-2)
                                                                           (assoc-in [:class->munged
                                                                                      magic-name]
                                                                                     (str prefix
                                                                                          seed-2))))))))
                                                       path-munged)))))]
    (assoc @v*ctx
           :output
           output-2)))

;;;;;;;;;; IO


(defn open-file+

  "Opens file paths.
  
   Returns a map containing of `path` to `map` containing:

   - `:content` -> string representing file content
   - Everything returned by [[detect-tag+]] after running it on the content"

  [path+]

  (into {}
        (for [path path+]
          (let [content (slurp path)]
            [path
             (assoc (detect-tag+ content)
                    :content
                    content)]))))



(defn write-file+

  "In files previously opened with [[open-file+]], substitutes CSS classes and vars for their
   munged names.
  
   Used by [[process!]]."

  ;; TODO. Warn about used names which does not have any rules.

  [opened-file+ {:keys [class->munged
                        var->munged]}]
  
  (run! (fn [[path {:keys [content]}]]
          (spit path
                (clojure.string/replace content
                                        regex-tagged
                                        (fn [magic-name]
                                          (or ((if (css-var? magic-name)
                                                 var->munged
                                                 class->munged)
                                               magic-name)
                                              "")))))
        opened-file+))



(defn file!

  "Makes a File object out of a string `path` and ensures its parent directory exist.
  
   Needed prior to writing such a file."

  ^File

  [path]

  (let [file (File. path)]
    (some-> (.getParentFile file)
            .mkdirs)
    file))


;;;;;;;;;; Centralizing previous optimizing steps


(defn process!

  "Processes through all the various optimizing steps described in this namespace.

   Used by [[main]]."

  [{:as             ctx
    :fcss.path/keys [cljs+
                     report]}]

  (let [opened-file+              (open-file+ cljs+)
        {:as   ctx-2
         :keys [original->munged+
                rule+]}           (-> (atomize-rule+ (assoc ctx
                                                            :detected-name+
                                                            (into #{}
                                                                  (mapcat :class+)
                                                                  (vals opened-file+))))
                                      group-decl+
                                      munge-class+
                                      rule-complex+
                                      class-join-munged
                                      (munge-var+ (into #{}
                                                        (mapcat :var+)
                                                        (vals opened-file+)))
                                      compile-rule+
                                      rename-in-output)]
    (when report
      (clojure.pprint/pprint ctx-2
                             (clojure.java.io/writer (file! report))))
    (write-file+ opened-file+
                 ctx-2)
    ctx-2))


;;;;;;;;;; Main function for compiling optimized CSS


(def main-default

  "Default arguments for [[main]]"

  {:fcss/operation   :release
   :fcss/prefix      "_-_"
   :fcss.path/cljs+  ["resources/public/js/main.js"]
   :fcss.path/output "resources/css/main.css"
   :fcss.path/report "resources/report/fcss.edn"})



(defn main

  "Entry point to optimizing compiler.
  
   Attention, alters the var root [[dev?]] to false."

  [arg+]

  (alter-var-root #'dev?
                  (constantly false))
  (let [{:as         arg-2+
         ns-entry    :fcss/entry
         path-output :fcss.path/output} (merge main-default
                                               arg+)]
    (require ns-entry
             :reload-all)
    (let [rule+  (vec (apply concat
                             (mapcat vals
                                     (vals @*rule+))))
          ctx    (merge arg-2+
                        {:fcss.rule/initial+ rule+
                         :rule+              rule+})
          ctx-2  (process! ctx)]
      (spit (file! path-output)
            (ctx-2 :output))
      ctx-2)))
