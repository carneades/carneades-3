;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.test-argument-search
  (:use clojure.pprint
        carneades.engine.argument-builtins
        carneades.engine.shell
        carneades.engine.argument-search
        carneades.engine.rule
        carneades.engine.search
        carneades.mapcomponent.viewer)
  (:require [carneades.engine.argument :as arg]))

(def rb
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

(def ag1
     (arg/accept arg/*empty-argument-graph*
                 facts))

(def generators 
        (list (generate-arguments-from-rules rb) 
              (builtins)))

(def max-nodes 20)
(def max-turns 3)

(defn engine
  ([rb ag max-nodes max-turns]
     (make-engine max-nodes max-turns ag
                   (list (generate-arguments-from-rules rb) (builtins)))))

; (def e1 (make-engine max-nodes max-turns ag1 generators))
(def e1 (engine rb ag1 20 3))
                     
(defn show [issue]
  (view (argue issue 20 3 facts generators)))     

; (ask e1 '(money ?x))
; (ask e1 '(movable ?x))
; (ask e1 '(goods ?x))
; (show '(money ?x))
; (show '(goods ?x))

(let [rb (rulebase
           (rule r1 
                 (if (and (movable ?c)
                          (unless (money ?c)))
                   (goods ?c))))
      ag (arg/accept arg/*empty-argument-graph*
                     '((movable item1)))
      eng (engine rb ag 20 2)
      ; eng (make-engine max-nodes max-turns ag generators)
      query '(goods item1)]
  ; (succeed? query eng))
  (ask eng '(goods ?x)))


