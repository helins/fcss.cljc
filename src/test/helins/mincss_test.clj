(ns helins.mincss-test

  ""

  (:require [clojure.set]
            [clojure.test  :as t]
            [helins.mincss :as mincss]))


;;;;;;;;;;


(def class-name+

  ""

  (mapv #(mincss/magic-class "ns"
                             (str "var"
                                  %))
        (range 4)))



(def allow-list

  ""

  (into #{}
        class-name+))




(t/deftest regex+

  (t/is (boolean (re-matches mincss/regex-magic
                             (class-name+ 0)))
        "Matches class name")
  (t/is (boolean (re-matches mincss/regex-magic-class
                             (mincss/dotted (class-name+ 0))))
        "Matches dotted class name"))



(t/deftest str->class-name+

  (t/is (= #{(class-name+ 0)
             (class-name+ 1)}
           (mincss/str->class-name+ (str "fldksjflskdjf"
                                         (class-name+ 0)
                                         "sldfkjsdlfksjkl"
                                         (class-name+ 1)
                                         "lmklfk,l")))))



(def rule-complex

  ""

  (mincss/rule (str (class-name+ 0)
                    ":hover")
               {:background 'red}))



(def rule-complex-remaining

  ""

  (mincss/rule (str (class-name+ 2)
                    ":hover")
               {:background 'red}))



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
                         rule-complex-remaining
                         rule-untouched]
                        allow-list))





(t/deftest atomize-rule+

  (let [{:keys [class->rule-complex+
                decl->class+
                rule+]}              ctx-atomize-rule+]
    (t/is (= {[:background 'black] #{(class-name+ 0)
                                     (class-name+ 1)
                                     (class-name+ 2)}
              [:color 'black]      #{(class-name+ 0)}
              [:color 'white]      #{(class-name+ 1)}
              [:margin 0]          #{(class-name+ 0)}}
             decl->class+)
          "CSS declarations are atomized")
    (t/is (= {(class-name+ 0) [rule-complex]
              (class-name+ 2) [rule-complex-remaining]}
             class->rule-complex+)
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

  (let [{:keys [class->rule-complex+
                original->munged+
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
               (second rule-complex)
               (second rule-untouched)}
             (into #{}
                   (map second)
                   rule+))
          "Declarations are regrouped in the rule collection")
    (t/is (= 1
             (count class->rule-complex+))
          "One complex rule is remaining")))



(def ctx-process-complex

  ""

  (mincss/process-complex ctx-rename-class+))



(t/deftest process-complex

  (t/is (empty? (ctx-process-complex :class->rule-complex+))
        "Remaining complex rule was processed")
  (t/is (= (inc (count (ctx-rename-class+ :rule+)))
           (count (ctx-process-complex :rule+)))
        "Complex rule is now ready"))
