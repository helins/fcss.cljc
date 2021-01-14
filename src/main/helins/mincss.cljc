(ns helins.mincss

  ""

  (:require [clojure.string]
            [helins.mincss.util :as mincss.util])
  #?(:cljs (:require-macros [helins.mincss])))


;;;;;;;;;;




#?(:clj



(do



(def magic-word-begin

  ""

  "__MINCSS_MAGIC_WORD_BEGIN__")



(def magic-word-end

  ""

  "__MINCSS_MAGIC_WORD_END__")



(def default-prefix

  ""

  "__HMC")






(defn sel

  ""

  ([class-name]

   (str \.
        class-name))


  ([selector placeholder->class-name]

   (reduce-kv (fn [selector-2 placeholder class-name]
                (clojure.string/replace selector-2
                                        (cond
                                          (keyword? placeholder) (name placeholder)
                                          (string? placeholder)  placeholder
                                          (symbol? placeholder)  (str placeholder))
                                        (sel class-name)))
              selector
              placeholder->class-name)))


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



(defn rule

  ""

  ([class-name style]

   [(sel class-name)
    style])

  ([selector placeholder->class-name style]

   [(sel selector
         placeholder->class-name)
    style]))






(defn str->magic+

  ""

  [string]

  (reduce (fn [acc magic-name]
            (update acc
                    (if (clojure.string/starts-with? magic-name
                                                     "--")
                      :var+
                      :class+)
                    conj
                    magic-name))
          {:class+ #{}
           :var+   #{}}
          (re-seq regex-magic
                  string)))




(defn atomize-rule+

  ""

  [rule+ allow-list]

  (reduce (fn [ctx [str-selector+ decl+ :as rule]]
            (if-some [dotted-class-name (re-matches regex-magic-dotted-class
                                                    str-selector+)]
              (let [class-name (.substring ^String dotted-class-name
                                           1)]
                (if (contains? allow-list
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
                (if (= (count (filter allow-list
                                      class-name+))
                       (count class-name+))

                  (update ctx
                          :rule-complex+
                          conj
                          {:class-name+ class-name+
                           :decl+       decl+
                           :selector    str-selector+})
                  ctx)
                (update ctx
                        :rule+
                        conj
                        rule))))
          {:decl->class+  {}
           :rule+         []
           :rule-complex+ []}
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
                                                [(sel munged-class)
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



(defn rename-var+

  ""

  [{:as   ctx
    :keys [var->munged]}]

  (if (seq var->munged)
    (update ctx
            :rule+
            (fn [rule+]
              (map (fn [[selector decl+ :as rule]]
                     [selector
                      (reduce-kv (fn [decl-2+ property value]
                                   (assoc decl-2+
                                          (cond->
                                            property
                                            (string? property)
                                            (clojure.string/replace regex-magic-var
                                                                    var->munged))
                                          (cond->
                                            value
                                            (string? value)
                                            (clojure.string/replace regex-magic-var
                                                                    var->munged))))
                                 {}
                                 decl+)])
                   rule+)))
    ctx))






(def advanced?

  ""

  (mincss.util/advanced?))



(defn magic

  ""

  [str-namespace str-sym]

  (let [class-name (str (clojure.string/replace str-namespace
                                                "."
                                                "__")
                        "__"
                        str-sym)]
    (if advanced?
      (str magic-word-begin
           class-name
           magic-word-end)
      class-name)))




(defn- -defname

  ;;

  [sym+ prefix]

  (let [str-ns (str *ns*)
        def+   (map #(list 'def
                           %
                           (str prefix
                                (magic str-ns
                                       (name %))))
                    sym+)]
    (if (= (count def+)
           1)
      (first def+)
      `(do
         ~@def+))))



(defmacro defclass

  ""

  [& sym+]

  (-defname sym+
            nil))



(defmacro defvar

  ""

  [& sym+]

  (-defname sym+
            "--"))



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



(defn write-file+

  ""

  [opened-file+ original->munged-str]
  
  (run! (fn [[path {:keys [content
                           class+]}]]
          (spit path
                (reduce #(clojure.string/replace %1
                                                 %2
                                                 (original->munged-str %2))
                        content
                        class+)))
        opened-file+))



(defn process!

  ""

  [path+ rule+]

  (if advanced?
    (let [opened-file+              (open-file+ path+)
          {:as   ctx
           :keys [original->munged+
                  rule+]}           (-> (atomize-rule+ rule+
                                                       (into #{}
                                                             (mapcat :class+
                                                                     (vals opened-file+))))
                                        group-decl+
                                        rename-class+
                                        process-complex
                                        (munge-var+ (into #{}
                                                          (mapcat :var+
                                                                  (vals opened-file+))))
                                        rename-var+)]
      (write-file+ opened-file+
                   (into {}
                         (map #(update %
                                       1
                                       (fn [munged+]
                                         (clojure.string/join " "
                                                              munged+))))
                         original->munged+))
      ctx)
    {:rule+ rule+}))




))
