;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.rule-examples
  (:use clojure.pprint
        carneades.engine.argument-builtins
        carneades.engine.shell
        carneades.engine.rule
        carneades.ui.diagram.viewer)
  (:require [carneades.engine.argument :as arg]))

(def rb1
     (rulebase
      
      (rule r1 
            (if (and (movable ?c)
                     (unless (money ?c)))
              (goods ?c)))
   
      (rule r3 (if (coins ?x) (money ?x)))
   
      
      ;; r4 and r5 are to test multiple conclusions
      (rule r4
            (if (movable ?x)
              (and (light ?x)
                   (shipable ?x))))
   
      (rule r5
            (if (and (light ?x)
                     (shipable ?x))
              (convenient ?x)))
   
      (rule* lex-posterior
             (if (and (enacted ?r1 ?d1)
                      (enacted ?r2 ?d2)
                      (later ?d2 ?d1))
               (prior ?r2 ?r1)))
   
      (rule r6 
            (if (edible ?x) 
              (not (goods ?x))))
   
      ;; to test exclusionary rules
      (rule r14 (if (bird ?x) (flies ?x)))
      (rule* r15 (if (penguin ?x) (excluded r14 (flies ?x))))
   
   
      ;; to test negative exceptions, by asking the validity critical question.
      (rule repeal
            (if (repealed ?r)
              (not (valid ?r))))
   
      ;; disjunction tests
      (rule r19 
            (if (or (p1 ?x) 
                    (p2 ?x)) 
              (p3 ?x)))
   
      (rule r20 
            (if (or (and (p4 ?x) (p5 ?x))
                    (and (p6 ?x) (p7 ?x))
                    (p8 ?x))
              (p9 ?x)))
   
      ;; eval test
      (rule r21 
            (if (and (p10 ?x) (eval ?z (reverse ?x))) 
              (p11 ?z)))
   
      ;; another eval test, to demonstrate calculations
      (rule r22
            (if (and (income ?x ?i)
                     (deductions ?x ?d)
                     (eval ?t (- ?i ?d)))
              (taxable-income ?x ?t)))
   
      )) ;; of rule base

; accept some facts 
(def ag1
     (arg/accept arg/*empty-argument-graph*
                 '((coins item1)
                   (bird Tweety)
                   (penguin Tweety)
                   (enacted r1 d1)
                   (enacted r6 d2)
                   (later d2 d1)
                   (repealed r5)
                   (movable item1)
                   (movable item2)
                   (edible item2)
                   (foo2 a)
                   (p1 a)
                   (p6 a)
                   (p7 a)
                   (p8 a)
                   (p10 '(a b c d e))
                   (income Sam 60000)
                   (deductions Sam 7000)

                   )))

(defn engine [max-nodes max-turns]
  (make-engine* max-nodes max-turns ag1
                (list (generate-arguments-from-rules rb1) (builtins))))

(defn -main []
 (time
  (do
    (printf "succeed? %s\n"(succeed? '(bird Tweety) (engine 20 1 [])))
    (printf "succeed? %s\n" (succeed? '(bird ?x) (engine 20 1 [])))
    (printf "succeed? %s\n" (succeed? '(money item1) (engine 20 1 [])))
    (printf "succeed? %s\n" (succeed? '(prior ?r1 ?r2) (engine 20 1 [])))
    (printf "succeed? %s\n" (succeed? '(p3 a) (engine 20 1 [])))
    (printf "succeed ? %s\n" (succeed? '(p9 a) (engine 20 1 [])))
    (printf "fail? %s\n" (fail? '(goods item1) (engine 20 2 [])))
    (printf "fail? %s\n" (fail? '(goods item2) (engine 20 3 [])))
    (printf "fail? %s\n" (fail? '(convenient item1) (engine 20 2 '(valid))))
    (printf "fail? %s\n" (fail? '(goods item2) (engine 20 2 '(priority))))
    (printf "succeed? %s\n" (succeed? '(not (goods item2)) (engine 20 3 [])))
    (printf "fail? %s\n" (fail? '(flies Tweety) (engine 20 2 '(excluded))))
    (printf "succeed? %s\n" (succeed? '(applies ?r (goods ?x)) (engine 20 2 [])))
    (printf "succeed? %s\n" (succeed? '(p11 ?x) (engine 20 1 [])))
    (printf "succeed? %s\n" (succeed? '(taxable-income Sam ?x) (engine 20 1 []))))))
