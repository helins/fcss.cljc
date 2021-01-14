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



(def ret-atomize-rule+

  ""

  (mincss/atomize-rule+ [(mincss/rule (class-name+ 0)
                                      {:background 'black
                                       :color      'black
                                       :margin     0})
                         (mincss/rule (class-name+ 1)
                                      {:background 'black
                                       :color      'white})
                         (mincss/rule "untouched"
                                      {:color 'red})]
                        allow-list))





(t/deftest atomize-rule+

  (let [{:keys [decl->class+
                untouched]}  ret-atomize-rule+]
    (t/is (= {[:background 'black] #{(class-name+ 0)
                                     (class-name+ 1)}
              [:color 'black]      #{(class-name+ 0)}
              [:color 'white]      #{(class-name+ 1)}
              [:margin 0]          #{(class-name+ 0)}}
             decl->class+))
    (t/is (= [[".untouched"
               {:color 'red}]]
             untouched))))



(def class+->style

  ""

  (mincss/group-decl+ (ret-atomize-rule+ :decl->class+)))



(t/deftest group-decl+

  (t/is (= {#{(class-name+ 0)} {:color  'black
                                :margin 0}
            #{(class-name+ 1)} {:color 'white}
            #{(class-name+ 0)
              (class-name+ 1)} {:background 'black}}
           class+->style)))



(t/deftest rename-class+

  (let [{:keys [original->munged+
                rule+
                seed]}            (mincss/rename-class+ class+->style
                                                        0)
        ]
    (t/is (= 3
             seed)
          "3 munged classes are created")
    (t/is (every? #(= %
                      2)
                  (map count
                       (vals original->munged+)))
          "Each original class should have one munged name for itself and one for a common class")
    (t/is (= 1
             (count (apply clojure.set/intersection
                           (map set
                                (vals original->munged+)))))
          "Each original class should share a common munged class")
    (t/is (= #{{:background 'black}
               {:color 'white}
               {:color  'black
                :margin 0}}
             (into #{}
                   (map second)
                   rule+))
          "Declarations are regrouped as they were")
    )
  )
