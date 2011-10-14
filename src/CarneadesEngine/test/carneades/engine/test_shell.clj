;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.test-shell
  (:use clojure.test
        clojure.contrib.pprint
        carneades.engine.shell
        carneades.engine.rule
        carneades.engine.argument-builtins
        carneades.ui.diagram.viewer)
  (:require [carneades.engine.argument :as arg]))

(defn engine
  ([rb ag max-nodes max-turns]
     (make-engine max-nodes max-turns ag
                   (list (generate-arguments-from-rules rb) (builtins)))))

(deftest test-engine-01-fact
  (let [rb (rulebase)
        ag (arg/accept arg/*empty-argument-graph*
                       '((bird Tweety)))
        eng (engine rb ag 20 1)
        query '(bird Tweety)]
    (is (succeed? query eng))))

(deftest test-engine-02-variable
  (let [rb (rulebase)
        ag (arg/accept arg/*empty-argument-graph*
                       '((bird Tweety)))
        eng (engine rb ag 20 1)
        query '(bird ?x)]
    (is (succeed? '(bird ?x) eng))))

(deftest test-engine-03-rule
  (let [rb (rulebase (rule r3 (if (coins ?x) (money ?x))))
        ag (arg/accept arg/*empty-argument-graph* '((coins item1)))
        eng (engine rb ag 20 1)
        query '(money item1)]
    (is (succeed? query eng))))

(deftest test-engine-04-conjunction
  (let [rb (rulebase (rule* lex-posterior
                            (if (and (enacted ?r1 ?d1)
                                     (enacted ?r2 ?d2)
                                     (later ?d2 ?d1))
                              (prior ?r2 ?r1))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((enacted r1 d1)
                         (enacted r2 d2)
                         (later d2 d1)))
        eng (engine rb ag 20 1)
        query '(prior r2 r1)]
    (is (succeed? query eng))))

(deftest test-engine-05-disjunction-01
  (let [rb (rulebase 
            (rule r19 
                  (if (or (p1 ?x) 
                          (p2 ?x)) 
                    (p3 ?x))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((p1 a)))
                         eng (engine rb ag 20 1)
        query '(p3 a)]
    (is (succeed? query eng))))

(deftest test-engine-06-disjunction-02
  (let [rb (rulebase (rule r20 
                           (if (or (and (p4 ?x) (p5 ?x))
                                   (and (p6 ?x) (p7 ?x))
                                   (p8 ?x))
                             (p9 ?x))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((p6 a)
                         (p7 a)))
                         eng (engine rb ag 20 1)
        query '(p9 a)]
    (is (succeed? query eng))))

(deftest test-engine-07-unless1
  (let [rb (rulebase
            (rule r1 
                  (if (and (movable ?c)
                           (unless (money ?c)))
                    (goods ?c))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((movable item1)))
        eng (engine rb ag 20 2)
        query '(goods item1)]
    (is (succeed? query eng))))

(deftest test-engine-07-unless2
  (let [rb (rulebase
            (rule r1 
                  (if (and (movable ?c)
                           (unless (money ?c)))
                    (goods ?c)))
   
            ; also test multiple conclusions
            (rule r2 (if (coins ?x) 
                       (and (movable ?x) 
                            (money ?x)))))
        
        ag (arg/accept arg/*empty-argument-graph*
                       '((coins item1)))
        eng (engine rb ag 20 2)
        query '(goods item1)]
    (is (fail? query eng))))

(deftest test-engine-08-rebuttal
  (let [rb (rulebase
            (rule r1 
                  (if (and (movable ?c)
                           (unless (money ?c)))
                    (goods ?c)))
            
            (rule r6 
                  (if (edible ?x) 
                    (not (goods ?x)))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((movable item2)
                         (edible item2)))
        eng (engine rb ag 20 2)
        query '(goods item2)]
    (is (succeed? '(not (goods item2)) eng)) 
    (is (fail? '(goods item2) eng))))

(deftest test-engine-10-applies
  (let [rb (rulebase
            (rule r1 
                  (if (movable ?c)                           
                      (goods ?c)))

            (rule r2 
                  (if (edible ?x) 
                      (not (goods ?x))))

            (rule* lex-posterior
                   (if (and (applies ?r1 ?g)
                            (applies ?r2 (not ?g))
                            (enacted ?r1 ?d1)
                            (enacted ?r2 ?d2)
                            (later ?d2 ?d1))
                     (prior ?r2 ?r1 ?g))))
        
        ag (arg/accept arg/*empty-argument-graph*
                       '((movable item1)
                         (edible item1)
                         (enacted r1 d1)
                         (enacted r2 d2)
                         (later d2 d1)))
        eng (engine rb ag 20 1)
        query '(prior r2 r1 (goods item1))]
    (is (succeed? query eng))))

(deftest test-engine-11-negativequery
  (let [rb (rulebase
            (rule r1 
                  (if (and (movable ?c)
                           (unless (money ?c)))
                    (goods ?c)))

            (rule r6 
                  (if (edible ?x) 
                      (not (goods ?x)))))
        
        ag (arg/accept arg/*empty-argument-graph*
                       '((movable i1)
                         (edible i1)))
        eng (engine rb ag 20 1)
        query '(not (goods ?x))]
    (is (succeed? query eng))))

(deftest test-engine-14-eval
  (let [rb (rulebase
            (rule r21 
                  (if (and (p10 ?x) (eval ?z (reverse ?x))) 
                    (p11 ?z))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((p10 (a b c d e))))
        eng (engine rb ag 20 1)
        query '(p11 (e d c b a))]
    (println "test-engine-14-eval result: " (ask eng query))
    (is (succeed? query eng))))

(deftest test-engine-14-eval-calculations
  (let [rb (rulebase
            (rule r22
                  (if (and (income ?x ?i)
                           (deductions ?x ?d)
                           (eval ?t (- ?i ?d)))
                    (taxable-income ?x ?t))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((income Sam 60000)
                         (deductions Sam 7000)))
        eng (engine rb ag 20 1)
        query '(taxable-income Sam 53000)]
     (println "test-engine-14-eval-calculations result: " (ask eng query))
    (is (succeed? query eng))))

(deftest test-engine-15-builtin-equal
  (let [rb (rulebase
            (rule r-doctor
                  (if (= (Title ?x ?y) (Title ?x Doctor))
                    (Doctor ?x))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((Title Tom Doctor)
                         (Title Tom Professor)))
        eng (engine rb ag 20 1)
        query '(Doctor Tom)]
    (is (succeed? query eng))))

(deftest test-engine-16-builtin-notequal
  (let [rb (rulebase
            (rule r-militaryduty
                  (if (and (MilitaryStatus ?x ?y)
                        (not= ?y Exempted))
                    (Enrolled ?x))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((MilitaryStatus Lena Exempted)
                         (MilitaryStatus Joe Done)))
        eng (engine rb ag 20 1)
        query '(Enrolled Lena)
        query2 '(Enrolled Joe)]
    (is (fail? query eng))
    (is (succeed? query2 eng))))

