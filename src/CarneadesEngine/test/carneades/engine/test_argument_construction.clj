;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.test-argument-construction
  (:use clojure.test
        clojure.pprint
        carneades.engine.argument 
        carneades.engine.rule
        carneades.engine.argument-construction
        ; carneades.mapcomponent.viewer
        ))

(def rb1
  (rulebase
    
    ; r1 has an exception
    (rule r1 
          (if (and (movable ?c)
                   (unless (money ?c)))
            (goods ?c)))
    
    ; r2 can be used to undercut r1
    (rule r2 (if (coins ?x) 
                 (and (movable ?x)
                      (money ?x))))
    
    
    ; r3 is a rebutter of r1    
    (rule r3 
          (if (edible ?x)
            (and (movable ?x) 
                 (not (goods ?x)))))))

(def facts '((movable i1)  ; thus goods, due to r1
             (coins i2)    ; thus not goods, due to r2
             (edible i3))) ; thus not goods, due to r3
 
(def max-goals 2000)

(defn ask [query]
  (let [ag1 (-> (argument-graph (gensym "ag") "" query)
                (accept facts))
        generators  (list (generate-arguments-from-rules rb1))]
    (matching-in-statements (construct-arguments ag1 query max-goals facts generators) 
                            query)))
                                                     
; (ask '(goods ?x))
; (ask '(not (goods ?x)))
; (show '(goods ?x))

