;;; Copyright (c) 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.test-shell
  (:use clojure.test
        carneades.engine.shell
        carneades.engine.rule))

(defn engine
  ([rb facts max-goals]
    (make-engine max-goals facts
                 (list (generate-arguments-from-rules rb)))))

(def max-goals 50)

(deftest test-engine-fact
         (let [rb (rulebase)
               facts '((bird Tweety))
               eng (engine rb facts max-goals)
               query '(bird Tweety)]
           (is (succeed? eng query #{query}))))

(deftest test-engine-variable
         (let [rb (rulebase)
               facts '((bird Tweety))
               eng (engine rb facts max-goals)
               query '(bird ?x)]
           (is (succeed? eng query #{'(bird Tweety)}))))

(deftest test-engine-rule
         (let [rb (rulebase (rule r1 (if (coins ?x) (money ?x))))
               facts '((coins item1))
               eng (engine rb facts max-goals)
               query '(money ?x)]
           (is (succeed? eng query #{'(money item1)}))))

(deftest test-engine-conjunction
         (let [rb (rulebase (rule lex-posterior
                                   (if (and (enacted ?r1 ?d1)
                                            (enacted ?r2 ?d2)
                                            (later ?d2 ?d1))
                                     (prior ?r2 ?r1))))
               facts '((enacted r1 d1)
                       (enacted r2 d2)
                       (later d2 d1))
               eng (engine rb facts max-goals)
               query '(prior ?x ?y)]
           (is (succeed? eng query #{'(prior r2 r1)}))))

(deftest test-engine-disjunction
         (let [rb (rulebase  (rule r19 (if (or (p1 ?x) 
                                               (p2 ?x)) 
                                         (p3 ?x))))
               facts '((p1 a))
               eng (engine rb facts max-goals)
               query '(p3 ?x)]
           (is (succeed? eng query #{'(p3 a)}))))

(deftest test-engine-dnf
         (let [rb (rulebase (rule r20 
                                  (if (or (and (p4 ?x) (p5 ?x))
                                          (and (p6 ?x) (p7 ?x))
                                          (p8 ?x))
                                    (p9 ?x))))
               facts '((p6 a) (p7 a))
               eng (engine rb facts max-goals)
               query '(p9 ?x)]
           (is (succeed? eng query #{'(p9 a)}))))

(deftest test-engine-unless1
         (let [rb (rulebase
                    (rule r1 
                          (if (and (movable ?c)
                                   (unless (money ?c)))
                            (goods ?c))))
               facts '((movable item1))
               eng (engine rb facts max-goals)
               query '(goods ?x)]
           (is (succeed? eng query #{'(goods item1)}))))

(deftest test-engine-unless2
         (let [rb (rulebase
                    (rule r1 
                          (if (and (movable ?c)
                                   (unless (money ?c)))
                            (goods ?c)))
                    ; also test multiple conclusions
                    (rule r2 (if (coins ?x) 
                               (and (movable ?x) 
                                    (money ?x)))))
               facts '((coins item1))
               eng (engine rb facts max-goals)
               query '(goods ?x)]
           (is (not (contains? (ask eng query) 
                               '(goods item1))))))

(deftest test-engine-rebuttal
         (let [rb (rulebase
                    (rule r1 (if (movable ?c) (goods ?c)))
                    (rule r2 (if (edible ?x) (not (goods ?x)))))
               facts '((movable item1)
                       (edible item1))
               eng (engine rb facts max-goals)
               query '(goods ?x)]
           (is (not (contains? (ask eng query) 
                               '(goods item1))))))

(deftest test-engine-applies
         (let [rb (rulebase
                    (rule r1 (if (movable ?c)  (goods ?c)))
                    
                    (rule r2 (if (edible ?x) (not (goods ?x))))
                    
                    (rule* lex-posterior
                           (if (and (applies ?r1 ?g)
                                    (applies ?r2 (not ?g))
                                    (enacted ?r1 ?d1)
                                    (enacted ?r2 ?d2)
                                    (later ?d2 ?d1))
                             (prior ?r2 ?r1 ?g))))
               
               facts '((movable item1)
                       (edible item1)
                       (enacted r1 d1)
                       (enacted r2 d2)
                       (later d2 d1))
               eng (engine rb facts max-goals)
               query '(prior r2 r1 (goods item1))]
           (is (succeed? eng query #{query}))))

(deftest test-engine-negative-query
         (let [rb (rulebase
                    (rule r1 (if (edible ?x) (not (goods ?x)))))
               facts '((edible i1))
               eng (engine rb facts max-goals)
               query '(not (goods ?x))]
           (is (succeed? eng query #{'(not (goods i1))}))))

(deftest test-engine-eval1
         (let [rb (rulebase
                    (rule r1 (if (eval ?y (reverse ?x)) 
                                 (rev ?x ?y))))
               facts ()
               eng (engine rb facts max-goals)
               query '(rev '(1 2 3 4) ?y)]
           ; to do: find some way to modify eval so that the list being
           ; reversed doesn't need to be quoted
           (is (succeed? eng query #{'(rev '(1 2 3 4) (4 3 2 1))}))))

(deftest test-engine-eval2
         (let [rb (rulebase
                    (rule r1 (if (and (income ?x ?i)
                                      (deductions ?x ?d)
                                      (eval ?t (- ?i ?d)))
                                  (taxable-income ?x ?t))))
               facts '((income Sam 60000)
                       (deductions Sam 7000))
               eng (engine rb facts max-goals)
               query '(taxable-income Sam ?r)]
           (is (succeed? eng query #{'(taxable-income Sam 53000)}))))

(deftest test-engine-equal
         (let [rb (rulebase
                    (rule r1 (if (and (title ?x ?y)
                                      (= ?y Dr))
                                 (has-phd ?x))))
               facts '((title Joe Dr))
               eng (engine rb facts max-goals)
               query '(has-phd ?x)]
           (is (succeed? eng query #{'(has-phd Joe)}))))

(deftest test-engine-not-equal
         (let [rb (rulebase
                    (rule r1
                          (if (and (status ?x ?y)
                                   (not= ?y exempted))
                            (enrolled ?x))))
               facts '((status Lea exempted)
                       (status Joe active))
               eng (engine rb facts max-goals)
               query '(enrolled ?x)]
           (is (succeed? eng query #{'(enrolled Joe)}))))


(deftest test-engine-cyclic-rules
         ; To test whether the argument construction 
         ; module terminates when rules are recursive
         ; with no base cases.  
         (let [rb (rulebase 
                    (rule r1 (if (foo ?x) (bar ?x)))
                    (rule r2 (if (bar ?x) (foo ?x))))
               eng (engine rb () max-goals)
               query '(foo ?x)]
           (is (succeed? eng query #{}))))

    
    


