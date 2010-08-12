;;; Copyright © 2010 Fraunhofer Gesellschaft 
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
     (engine rb ag max-nodes max-turns []))
  ([rb ag max-nodes max-turns critical-questions]
     (make-engine* max-nodes max-turns ag
                   (list (generate-arguments-from-rules rb critical-questions)
                         builtins))))

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

(deftest test-engine-04-prior
  (let [rb (rulebase (rule* lex-posterior
                            (if (and (enacted ?r1 ?d1)
                                     (enacted ?r2 ?d2)
                                     (later ?d2 ?d1))
                              (prior ?r2 ?r1))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((enacted r1 d1)
                         (enacted r6 d2)
                         (later d2 d1)))
                         eng (engine rb ag 20 1)
        query '(prior ?r1 ?r2)]
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
                         (p7 a)
                         (p8 a)))
                         eng (engine rb ag 20 1)
        query '(p9 a)]
    (is (succeed? query eng))))

(deftest test-engine-07-unless
  (let [rb (rulebase
            (rule r1 
                  (if (and (movable ?c)
                           (unless (money ?c)))
                    (goods ?c)))
   
            (rule r3 (if (coins ?x) (money ?x))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((coins item1)
                         (movable item1)))
                         eng (engine rb ag 20 2)
        query '(goods item1)]
    (is (fail? query eng))))

(deftest test-engine-08-negativeconclusion
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
    (is (fail? query eng))))

(deftest test-engine-09-critical-question
  (let [rb (rulebase
            (rule r4
                  (if (movable ?x)
                    (and (light ?x)
                         (shipable ?x))))
            
            (rule r5
                  (if (and (light ?x)
                           (shipable ?x))
                    (convenient ?x)))

             (rule repeal
                   (if (repealed ?r)
                     (not (valid ?r)))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((coins item1)
                         (movable item1)
                         (repealed r5)))
        eng (engine rb ag 20 2 '(valid))
        query '(convenient item1)]
    (is (fail? query eng))))

(deftest test-engine-10-lexposterior
  (let [rb (rulebase
            (rule r1 
                  (if (and (movable ?c)
                           (unless (money ?c)))
                    (goods ?c)))

            (rule r6 
                  (if (edible ?x) 
                    (not (goods ?x))))

            (rule* lex-posterior
                   (if (and (enacted ?r1 ?d1)
                            (enacted ?r2 ?d2)
                            (later ?d2 ?d1))
                     (prior ?r2 ?r1))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((movable item2)
                         (edible item2)
                         (enacted r1 d1)
                         (enacted r6 d2)
                         (later d2 d1)))
        eng (engine rb ag 20 1 '(priority))
        query '(goods item2)]
    (is (fail? query eng))))

(deftest test-engine-11-negativequery
  (let [rb (rulebase
            (rule r1 
                  (if (and (movable ?c)
                           (unless (money ?c)))
                    (goods ?c)))

            (rule r6 
                  (if (edible ?x) 
                    (not (goods ?x))))

            (rule* lex-posterior
                   (if (and (enacted ?r1 ?d1)
                            (enacted ?r2 ?d2)
                            (later ?d2 ?d1))
                     (prior ?r2 ?r1))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((movable item2)
                         (edible item2)
                         (enacted r1 d1)
                         (enacted r6 d2)
                         (later d2 d1)))
        eng (engine rb ag 20 1)
        query '(not (goods item2))]
    (is (succeed? query eng))))

(deftest test-engine-12-excluded
  (let [rb (rulebase
            (rule r14 (if (bird ?x) (flies ?x)))
            (rule* r15 (if (penguin ?x) (excluded r14 (flies ?x)))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((bird Tweety)
                         (penguin Tweety)))
        eng (engine rb ag 20 2 '(excluded))
        query '(flies Tweety)]
    (is (fail? query eng))))

(deftest test-engine-13-applies
  (let [rb (rulebase
            (rule r1 (if (and (movable ?c)
                      (unless (money ?c)))
               (goods ?c)))
            
            (rule r3 (if (coins ?x) (money ?x))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((coins item1)
                         (money item1)
                         (movable item2)
                         (edible item2)))
        eng (engine rb ag 20 1)
        query '(applies ?r (goods ?x))]
    (is (succeed? query eng))))

(deftest test-engine-14-eval
  (let [rb (rulebase
            (rule r21 
                  (if (and (p10 ?x) (eval ?z (reverse ?x))) 
                    (p11 ?z))))
        ag (arg/accept arg/*empty-argument-graph*
                       '((p10 '(a b c d e))))
        eng (engine rb ag 20 1)
        query '(p11 ?x)]
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
        query '(taxable-income Sam ?x)]
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

;; (view (:arguments (first (solutions (eng query)))))

