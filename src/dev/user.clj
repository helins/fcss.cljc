(ns user

  "For daydreaming at the REPL."

  (:require [helins.mincss :as mincss]))


;;;;;;;;;;


(require '[nrepl.server])  (defonce server (nrepl.server/start-server :port 4000))



(comment


  (mincss/defclass foo)


  (mincss/defclass bar)


  (def style+

    [[(mincss/dotted foo)
      {:background 'red
       :margin     "1em"}]

     [(mincss/dotted bar)
      {:background 'red
       :color      'green}]])



  (def prop->class+

    (mincss/atomize-style+ style+))


  (def class-set->style

    (mincss/regroup-class+ prop->class+))



  (def renamed

    (mincss/rename-class+ class-set->style
                          0))






  (mincss/process! ["/home/adam/projects/clj/helins/mincss/dev/test.txt"]
                   [(mincss/rule (mincss/magic-class "ns"
                                               		 "var")
                                 {:background 'red
                                  :color      'black})
                    (mincss/rule (mincss/magic-class "ns"
                                               		 "var2")
                                 {:background 'red
                                  :color      'white})])


  )
