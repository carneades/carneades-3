;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.test-argument-construction
  (:use clojure.test
        clojure.pprint
        carneades.engine.shell
        carneades.engine.rule
        carneades.mapcomponent.viewer))

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
                 (not (goods ?x)))))
    
    (rule r4 (if (and (income ?x ?i)
                      (deductions ?x ?d)
                      (eval ?t (- ?i ?d)))
                  (taxable-income ?x ?t)))
    
  
    (rule lex-posterior
          (if (and (enacted ?r1 ?d1)
                   (enacted ?r2 ?d2)
                   (later ?d2 ?d1))
              (prior ?r2 ?r1)))))
              

(def facts '((movable i1)  ; thus goods, due to r1
             (coins i2)    ; thus not goods, due to r2
             (edible i3)   ; thus not goods, due to r3
             (income Sam 60000)
             (deductions Sam 7000)
             (enacted r1 d1)
             (enacted r2 d2)
             (later d2 d1))) 
 
(def max-goals 2000)

(def generators (list (generate-arguments-from-rules rb1)))

(def e1 (make-engine max-goals facts generators))
                                                     
; (ask e1 '(goods ?x))
; (ask e1 '(not (goods ?x)))
; (view (argue e1 '(money i2)))

; (view (argue e1 '(goods ?x)))

(view (argue e1 '(taxable-income Sam ?x)))

; (ask e1 '(money ?x))
