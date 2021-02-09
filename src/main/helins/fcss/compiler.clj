(ns helins.fcss.compiler

  ""

  (:require [clojure.java.io]
            [clojure.pprint]
            [clojure.string]
            [garden.core           :as garden]
            [garden.compiler]
            [helins.medium.co-load :as medium.co-load])
  (:import java.io.File))


;;;;;;;;;;


(def dev?

  ""

  true)



(def ^String dev-root

  ""

  "fcss/dev/fcss")


;;;;;;;;;; Miscellaneous


(defn css-var?

  ""

  [string]

  (clojure.string/starts-with? string
                               "--"))


;;;;;;;;;; Central registry for CSS rules


(defonce *rule+

  (atom {}))


(defn add-rule!

  ""

  [unqualified-sym rule+]

  (swap! *rule+
         assoc-in
         [(ns-name *ns*)
          unqualified-sym]
         rule+))


;;;;;;;;;; Tagging strings and working with them


(def tag-begin

  ""

  "<<<_FCSS_")



(def tag-end

  ""

  "_FCSS_>>>")


;;;;; Regexes for finding different kinds of tagged strings


(let [base-pattern (str tag-begin
                  	    "\\S+?"
                  	    tag-end)]

  (def regex-tagged

    ""

    (re-pattern (str "(?:--)?"
                     base-pattern)))


  (def regex-tagged-class

	""

	(re-pattern base-pattern))


  (def regex-tagged-var

    ""

    (re-pattern (str "--"
                     base-pattern)))


  (def regex-tagged-dotted-class

    ""
	
    (re-pattern (str "\\."
					 base-pattern))))



(defn detect-tag+

  ""

  [string]

  (reduce (fn [acc magic-name]
            (update acc
                    (if (css-var? magic-name)
                      :var+
                      :class+)
                    conj
                    magic-name))
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
                               unqualified-sym)
         sym-rul   (symbol var-rul)
         rule+     (get-in @*rule+
                           [(symbol (namespace sym-rul))
                            (symbol (name sym-rul))])]
     (when (= (-> rule+
                  meta
                  :fcss.co-load/compile-cycle)
              (medium.co-load/compile-cycle))
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
                           e))))))))


;;;;;;;;;; Compiling and optimizing CSS for release


(defn add-rule

  ""

  [ctx rule]

  (update ctx
          :rule+
          conj
          rule))



(defn atomize-rule+

  ""

  ;; TODO. Ensure declaration equality by already compiling them.
  ;; TODO. Provide genuine support for vector selectors.

  [{:as   ctx
    :keys [detected-name+
           rule+]}]

  (reduce (fn [ctx rule]
            (if (vector? rule)
              (let [[str-selector+
                     decl+ ]       rule

                    str-selector+  (cond->>
                                     str-selector+
                                     (vector? str-selector+)
                                     (clojure.string/join ","))]
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
              (add-rule ctx
                        rule)))
          (assoc ctx
                 :decl->class+  {}
                 :rule+         []
                 :rule-complex+ [])
          rule+))



(defn group-decl+

  ""

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



(defn rename-class+

  ""

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

  ""

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



(defn process-complex

  ""

  [{:as   ctx
    :keys [rule-complex+]}]

  (reduce (fn [ctx-2 {:keys [class-name+
                             decl+
                             selector]}]
            (let [ctx-3 (reduce ensure-unique
                                ctx-2
                                class-name+)
                  selector-2 (reduce (fn [selector-2 [class-name munged]]
                                       (clojure.string/replace selector-2
                                                               class-name
                                                               munged))
                                     selector
                                     (map (juxt identity
                                                (ctx-3 :class->unique-munged))
                                          class-name+))
                  ]
              (update ctx-3
                      :rule+
                      conj
                      [selector-2
                       decl+])))
          (dissoc ctx
                  :rule-complex+)
          rule-complex+))



(defn munge-var+

  ""

  [ctx var+]

  (let [{:fcss/keys [prefix]} ctx]
    (reduce (fn [ctx-2 magic-var]
              (let [seed-2 (inc (ctx-2 :seed))]
                (-> ctx-2
                    (assoc :seed
                           seed-2)
                    (update :var->munged
                            assoc
                            magic-var
                            (str "--"
                                 prefix
                                 seed-2)))))
            ctx
            var+)))



(defn compile-rule+

  ""

  [{:as   ctx
    :keys [rule+]}]

  (assoc ctx
         :output
         (garden/css rule+)))



(defn rename-in-output

  ""

  ;; TODO. Warn that a declaration refers to something that is not used in code (eg. css var not defined in advanced build).

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



(defn open-file+

  ""

  [path+]

  (into {}
        (for [path path+]
          (let [content (slurp path)]
            [path
             (assoc (detect-tag+ content)
                    :content
                    content)]))))




(defn join-munged

  ""

  [{:as   ctx
    :keys [original->munged+]}]

  (assoc ctx
         :class->munged
         (into {}
               (for [[class-name munged+] original->munged+]
                 [class-name
                  (clojure.string/join " "
                                       munged+)]))))


(defn write-file+

  ""

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

  ""

  ^File

  [path]

  (let [file (File. path)]
    (some-> (.getParentFile file)
            .mkdirs)
    file))




(defn process!

  ""

  [{:as             ctx
    :fcss.path/keys [cljs+
                     report]}]

  (let [opened-file+              (open-file+ cljs+)
        {:as   ctx-2
         :keys [original->munged+
                rule+]}           (-> (atomize-rule+ (assoc ctx
                                                            :detected-name+
                                                            (into #{}
                                                                  (mapcat :class+
                                                                          (vals opened-file+)))))
                                      group-decl+
                                      rename-class+
                                      process-complex
                                      join-munged
                                      (munge-var+ (into #{}
                                                        (mapcat :var+
                                                                (vals opened-file+))))
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

  ""

  {:fcss/operation   :release
   :fcss/prefix      "_-_"
   :fcss.path/cljs+  ["resources/public/js/main.js"]
   :fcss.path/output "resources/css/main.css"
   :fcss.path/report "resources/report/fcss.edn"})



(defn main

  ""

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
