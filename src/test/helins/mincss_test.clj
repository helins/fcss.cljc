(ns helins.mincss-test

  ""

  (:require [clojure.set]
            [clojure.test  :as t]
            [helins.mincss :as mincss]))


;;;;;;;;;;


(def class-name+

  ""

  (mapv #(mincss/magic "ns"
                       (str "var"
                            %))
        (range 4)))



(def allow-list

  ""

  (into #{}
        class-name+))



(t/deftest sel

  (t/is (= ".class"
           (mincss/sel "class"))
        "Arity 1")
  (t/is (= ".class-1:hover .class-2.class-3"
           (mincss/sel "$$1:hover $$2$$3"
                       {:$$1  "class-1"
                        '$$2  "class-2"
                        "$$3" "class-3"}))
        "Variadic"))


(t/deftest rule

  (t/is (= [".class"
            {:color 'red}]
           (mincss/rule "class"
                        {:color 'red}))
        "With a class name")
  (t/is (= [".class-1:hover .class-2"
            {:color 'red}]
           (mincss/rule "$1:hover $2"
                        {:$1 "class-1"
                         :$2 "class-2"}
                        {:color 'red}))
        "With placeholders"))



(t/deftest regex+

  (t/is (boolean (re-matches mincss/regex-magic
                             (class-name+ 0)))
        "Can match class")
  (t/is (boolean (re-matches mincss/regex-magic
                             (str "--"
                                  (class-name+ 0))))
        "Can match var")
  (t/is (boolean (re-matches mincss/regex-magic-class
                             (class-name+ 0)))
        "Matches class name")
  (t/is (boolean (re-matches mincss/regex-magic-dotted-class
                             (mincss/sel (class-name+ 0))))
        "Matches dotted class name"))



(t/deftest str->magic

  (t/is (= {:class+ #{(class-name+ 0)
                      (class-name+ 1)}
            :var+   #{(str "--"
                           (class-name+ 2))}}
           (mincss/str->magic+ (str "fldksjflskdjf"
                                    (class-name+ 0)
                                    "sldfkjsdlfksjkl"
                                    (class-name+ 1)
                                    "lmklfk,l"
                                    "--"
                                    (class-name+ 2)
                                    "lfdkslfkj")))))



(def rule-complex

  ""

  (mincss/rule (str (class-name+ 0)
                    ":hover")
               {:background 'red}))



(def rule-complex-nested

  ""

  (mincss/rule (str (class-name+ 0)
                    ":active "
                    (class-name+ 2))
               {:background 'green}))



(def rule-untouched

  ""

  (mincss/rule "untouched"
               {:color 'red}))



(def ctx-atomize-rule+

  ""

  (mincss/atomize-rule+ [(mincss/rule (class-name+ 0)
                                      {:background 'black
                                       :color      'black
                                       :margin     0})
                         (mincss/rule (class-name+ 1)
                                      {:background 'black
                                       :color      'white})
                         (mincss/rule (class-name+ 2)
                                      {:background 'black})
                         rule-complex
                         rule-complex-nested
                         rule-untouched]
                        allow-list))





(t/deftest atomize-rule+

  (let [{:keys [decl->class+
                rule+
                rule-complex+]} ctx-atomize-rule+]
    (t/is (= {[:background 'black] #{(class-name+ 0)
                                     (class-name+ 1)
                                     (class-name+ 2)}
              [:color 'black]      #{(class-name+ 0)}
              [:color 'white]      #{(class-name+ 1)}
              [:margin 0]          #{(class-name+ 0)}}
             decl->class+)
          "CSS declarations are atomized")
    (t/is (= #{{:class-name+ #{(class-name+ 0)}
                :decl+       (second rule-complex)
                :selector    (first rule-complex)}
               {:class-name+ #{(class-name+ 0)
                               (class-name+ 2)}
                :decl+       (second rule-complex-nested)
                :selector    (first rule-complex-nested)}}
             (into #{}
                   rule-complex+))
          "Complex rules are detected")
    (t/is (= [[".untouched"
               {:color 'red}]]
             rule+)
          "Rules not involving magic classes are detected")))



(def ctx-group-decl+

  ""

  (mincss/group-decl+ ctx-atomize-rule+))



(t/deftest group-decl+

  (t/is (= (assoc ctx-group-decl+
                  :class+->style
                  {#{(class-name+ 0)} {:color  'black
                                       :margin 0}
                   #{(class-name+ 1)} {:color 'white}
                   #{(class-name+ 0)
                     (class-name+ 1)
                     (class-name+ 2)} {:background 'black}})
           ctx-group-decl+)))



(def ctx-rename-class+

  ""

  (mincss/rename-class+ ctx-group-decl+))



(t/deftest rename-class+

  (let [{:keys [original->munged+
                rule+
                seed]}               ctx-rename-class+]
    (t/is (= 3
             seed)
          "3 munged classes are created")
    (t/is (= 1
             (count (apply clojure.set/intersection
                           (map set
                                (vals original->munged+)))))
          "Each original class should share a common munged class")
    (t/is (= #{{:background 'black}
               {:color 'white}
               {:color  'black
                :margin 0}
               (second rule-untouched)}
             (into #{}
                   (map second)
                   rule+))
          "Declarations are regrouped in the rule collection")))



(def ctx-process-complex

  ""

  (mincss/process-complex ctx-rename-class+))



(t/deftest process-complex

  (t/is (empty? (ctx-process-complex :rule-complex+))
        "No more pending complex rules")
  (t/is (= (+ (count (ctx-rename-class+ :rule+))
              (count (ctx-rename-class+ :rule-complex+)))
           (count (ctx-process-complex :rule+)))
        "Complex rule is now ready"))



(t/deftest munge-var+

  (t/is (= {:prefix      "P"
            :seed        2
            :var->munged {"a" "--P1"
                          "b" "--P2"}}
           (mincss/munge-var+ {:prefix "P"
                               :seed   0}
                              ["a" "b"]))))



(mincss/defvar var-1)
(mincss/defvar var-2)



(t/deftest rename-var+

  (t/is (= [["selector"
             {:background "var( --v1, red)"
              :color      'red
              "--v2"      'green}]]
           (-> (mincss/rename-var+ {:rule+       [["selector"
                                                   {:background (format "var( %s, red)"
                                                                        var-1)
                                                    :color      'red
                                                    var-2       'green}]]
                                    :var->munged {var-1 "--v1"
                                                  var-2 "--v2"}})
               :rule+))))
