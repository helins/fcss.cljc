(ns helins.fcss.compiler

  ""

  (:require [cljs.env]
            [clojure.string]

            [clojure.tools.namespace.repl]

            [garden.core     :as garden]
            [garden.compiler]))


;;;;;;;;;;


(clojure.tools.namespace.repl/disable-reload!)


(def ^:dynamic *defrul?*

  ;;

  false
  )



(def magic-word-begin

  ""

  "__MINCSS_MAGIC_WORD_BEGIN__")



(def magic-word-end

  ""

  "__MINCSS_MAGIC_WORD_END__")



(def default-prefix

  ""

  "_fcss")







(defn css-var?

  ""

  [string]

  (clojure.string/starts-with? string
                               "--"))



(let [base-pattern (str magic-word-begin
                  	    "\\S+?"
                  	    magic-word-end)]

  (def regex-magic

    ""

    (re-pattern (str "(?:--)?"
                     base-pattern)))

  (def regex-magic-class

	""

	(re-pattern base-pattern))


  (def regex-magic-var

    ""

    (re-pattern (str "--"
                     base-pattern)))


  (def regex-magic-dotted-class

    ""
	
    (re-pattern (str "\\."
					 base-pattern))))





(defn str->magic+

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
          (re-seq regex-magic
                  string)))



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
                (if-some [dotted-class-name (re-matches regex-magic-dotted-class
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
                                                         (re-seq regex-magic-class
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

  [{:as   ctx
    :keys [class+->style
           prefix]}]

  (let [prefix-2 (or prefix
                     default-prefix)]
    (reduce-kv (fn [ctx-2 class+ style]
                 (let [seed-2       (inc (ctx-2 :seed))
                       munged-class (str prefix-2
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
                          :original->munged+    {}
                          :prefix               prefix-2)

                   (update :seed
                           #(or %
                                0)))
               class+->style)))



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
            munged-class (str (ctx :prefix)
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

  (let [{:keys [prefix]} ctx]
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

  [{:as   ctx
    :keys [output
           prefix
           var->munged]}]

  (let [v*ctx    (volatile! ctx)
        output-2 (clojure.string/replace output
                                         regex-magic
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



(defn namespaced-name

  ""

  [str-ns str-sym]

  (str (clojure.string/replace str-ns
                               "."
                               "__")
       "__"
       str-sym))









(def cljs-optimization-level

  ""

  (some-> cljs.env/*compiler*
          deref
          (get-in [:options
                   :optimizations])))




(defn cljs-optimization-level-2

  ""

  []

  (some-> cljs.env/*compiler*
          deref
          (get-in [:options
                   :optimizations])))





(defn cljs?

  ""

  []

  (boolean (some-> cljs.env/*compiler*
                   deref)))



(defn cljs-optimization

  ""

  []

  (when-some [*cljs-compiler cljs.env/*compiler*]
    (if (identical? (get-in @*cljs-compiler
                            [:options
                             :optimizations])
                    :advanced)
      :release
      :dev)))






(defn compiling-cljs?

  ""

  []

  (boolean (some-> cljs.env/*compiler*
                   deref)))




(def dev?

  ""

  (or (nil? cljs-optimization-level)
      (not (identical? cljs-optimization-level
                       :advanced))))




(def release?

  ""

  (or (nil? cljs-optimization-level)
      (identical? cljs-optimization-level
                  :advanced)))







(defn add-magic-word+

  ""

  [string]

  (case (cljs-optimization)
    :dev string
    (str magic-word-begin
         string
         magic-word-end)))




(defn magic

  ""

  [str-ns str-sym]

  (add-magic-word+ (namespaced-name str-ns
                                    str-sym)))





(defn open-file+

  ""

  [path+]

  (into {}
        (for [path path+]
          (let [content (slurp path)]
            [path
             (assoc (str->magic+ content)
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

  [opened-file+ {:keys [class->munged
                        var->munged]}]
  
  (run! (fn [[path {:keys [content]}]]
          (spit path
                (clojure.string/replace content
                                        regex-magic
                                        (fn [magic-name]
                                          ((if (css-var? magic-name)
                                             var->munged
                                             class->munged)
                                           magic-name)))))
        opened-file+))



(defn process!

  ""

  [{:as   ctx
    :keys [path-cljs+]}]

  (let [opened-file+              (open-file+ path-cljs+)
        {:as   ctx
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
    (write-file+ opened-file+
                 ctx)
    ctx))
