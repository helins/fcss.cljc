(ns helins.mincss.compiler-test

  ""

  (:require [clojure.set]
            [clojure.test           :as t]
            [helins.mincss          :as mincss]
            [helins.mincss.compiler :as mincss.compiler]))


;;;;;;;;;;

(mincss/defclass cl-0)
(mincss/defclass cl-1)
(mincss/defclass cl-2)

(mincss/defvar var-0)



(def allow-list

  ""

  (into #{}
        (map str)
        [cl-0
         cl-1
         cl-2]))



(t/deftest regex+

  (t/is (boolean (re-matches mincss.compiler/regex-magic
                             (str cl-0)))
        "Can match class")
  (t/is (boolean (re-matches mincss.compiler/regex-magic
                             var-0))
        "Can match var")
  (t/is (boolean (re-matches mincss.compiler/regex-magic-class
                             (str cl-0)))
        "Matches class name")
  (t/is (boolean (re-matches mincss.compiler/regex-magic-dotted-class
                             (mincss/templ cl-0)))
        "Matches dotted class name"))



(t/deftest str->magic

  (t/is (= {:class+ #{(str cl-0)
                      (str cl-1)}
            :var+   #{var-0}}
           (mincss.compiler/str->magic+ (str "fldksjflskdjf"
                                             (str cl-0)
                                             "sldfkjsdlfksjkl"
                                             (str cl-1)
                                             "lmklfk,l"
                                             (str var-0)
                                             "lfdkslfkj")))))



(def rule-complex

  ""

  (mincss/rule "$$:hover"
               {:$$ cl-0}
               {:background 'red}))



(def rule-complex-nested

  ""

  (mincss/rule "$0:active $1"
               {:$0 cl-0
                :$1 cl-1}
               {:background 'green}))



(def rule-untouched

  ""

  (mincss/rule ".untouched"
               {:color 'red}))



(def ctx-atomize-rule+

  ""

  (mincss.compiler/atomize-rule+ [(mincss/rule cl-0
                                               {:background 'black
                                                :color      'black
                                                :margin     0})
                                  (mincss/rule cl-1
                                               {:background 'black
                                                :color      'white})
                                  (mincss/rule cl-2
                                               {:background 'black})
                                  rule-complex
                                  rule-complex-nested
                                  rule-untouched]
                                 allow-list))





(t/deftest atomize-rule+

  (let [{:keys [decl->class+
                rule+
                rule-complex+]} ctx-atomize-rule+]
    (t/is (= {[:background 'black] #{(str cl-0)
                                     (str cl-1)
                                     (str cl-2)}
              [:color 'black]      #{(str cl-0)}
              [:color 'white]      #{(str cl-1)}
              [:margin 0]          #{(str cl-0)}}
             decl->class+)
          "CSS declarations are atomized")
    (t/is (= #{{:class-name+ #{(str cl-0)}
                :decl+       (second rule-complex)
                :selector    (first rule-complex)}
               {:class-name+ #{(str cl-0)
                               (str cl-1)}
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

  (mincss.compiler/group-decl+ ctx-atomize-rule+))



(t/deftest group-decl+

  (t/is (= (assoc ctx-group-decl+
                  :class+->style
                  {#{(str cl-0)} {:color  'black
                                  :margin 0}
                   #{(str cl-1)} {:color 'white}
                   #{(str cl-0)
                     (str cl-1)
                     (str cl-2)} {:background 'black}})
           ctx-group-decl+)))



(def ctx-rename-class+

  ""

  (mincss.compiler/rename-class+ ctx-group-decl+))



(t/deftest rename-class+

  (let [{:keys [original->munged+
                rule+
                seed]}             ctx-rename-class+]
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

  (mincss.compiler/process-complex ctx-rename-class+))



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
           (mincss.compiler/munge-var+ {:prefix "P"
                                        :seed   0}
                                       ["a" "b"]))))
