(ns helins.mincss

  ""

  (:require [clojure.string])
  #?(:cljs (:require-macros [helins.mincss])))


;;;;;;;;;;


(def magic-word-begin

  ""

  "__MINCSS_MAGIC_WORD_BEGIN__")



(def magic-word-end

  ""

  "__MINCSS_MAGIC_WORD_END__")



(def default-prefix

  ""

  "__HMC")


(defn magic-class

  ""

  [str-namespace str-sym]

  (str magic-word-begin
       str-namespace
       "__"
       str-sym
       magic-word-end))



(defn dotted

  ""

  [class-name]

  (str \.
       class-name))



(let [base-pattern (str magic-word-begin
                  	    "\\S+?"
                  	    magic-word-end)]

  (def regex-magic

	""

	(re-pattern base-pattern))


  (def regex-magic-class

    ""
	
    (re-pattern (str "\\."
					 base-pattern))))



(defn rule

  ""

  [class-name style]

  [(dotted class-name)
   style])


(defmacro defclass

  ""

  ([sym]

   `(defclass ~sym
              nil))


  ([sym docstring]

   (concat ['def sym]
           (when docstring
             [docstring])
           [(magic-class (str *ns*)
                   		 (name sym))])))



(defn str->class-name+

  ""

  [string]

  (into #{}
        (re-seq regex-magic
                string)))



(defn atomize-rule+

  ""

  [rule+ allow-list]

  (reduce (fn [acc [str-selector+ decl+ :as rule]]
            (if-some [dotted-class-name (re-matches regex-magic-class
                                                    str-selector+)]
              (let [class-name (.substring ^String dotted-class-name
                                           1)]
                (if (contains? allow-list
                               class-name)
                  (update acc
                          :decl->class+
                          (fn [decl->class+]
                            (reduce #(update %1
                                             %2
                                             (fnil conj
                                                   #{})
                                             class-name)
                                    decl->class+
                                    decl+)))
                  acc))
              (update acc
                      :untouched
                      conj
                      rule)))
          {:decl->class+ {}
           :untouched    []}
          rule+))




(defn group-decl+

  ""

  [decl->class+]

  (reduce-kv (fn [class+->style decl class+]
               (update class+->style
                       class+
                       (fnil conj
                             {})
                       decl))
             {}
             decl->class+))



(defn rename-class+

  ""

  ([class+->style seed]

   (rename-class+ class+->style
                  nil
                  seed))


  ([class+->style prefix seed]

   (let [prefix-2 (or prefix
                      default-prefix)]
     (reduce-kv (fn [acc class+ style]
                  (let [seed-2       (inc (acc :seed))
                        munged-class (str prefix-2
                                          seed-2)]
                    (-> acc
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
                                [(dotted munged-class)
                                 style])
                        (assoc :seed
                               seed-2))))
                {:original->munged+ {}
                 :rule+             []
                 :seed              seed}
                class+->style))))




#?(:clj



(do


(defn open-file+

  ""

  [path+]

  (into {}
        (for [path path+]
          (let [content (slurp path)]
            [path
             {:content     content
              :class-name+ (str->class-name+ content)}]))))



(defn write-file+

  ""

  [opened-file+ original->munged-str]
  
  (run! (fn [[path {:keys [content
                           class-name+]}]]
          (spit path
                (reduce #(clojure.string/replace %1
                                                 %2
                                                 (original->munged-str %2))
                        content
                        class-name+)))
        opened-file+))



(defn process!

  ""

  [path+ rule+]

  (let [opened-file+              (open-file+ path+)
        {:keys [decl->class+
                untouched]}       (atomize-rule+ rule+
                                                 (into #{}
                                                       (mapcat :class-name+
                                                                (vals opened-file+))))
        class+->style             (group-decl+ decl->class+)
        {:keys [original->munged+
                rule+]}           (rename-class+ class+->style
                                                 0)
        ]
    (write-file+ opened-file+
                 (into {}
                       (map #(update %
                                     1
                                     (fn [munged+]
                                       (clojure.string/join " "
                                                            munged+))))
                       original->munged+))
    (concat untouched
            rule+)))

  
)



)
