;;; Copyright (c) 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.test-shell
  (:use clojure.test
        carneades.engine.shell
        carneades.engine.theory))

(def theory1 
  (make-theory
    :sections [(make-section 
                 :name "Birds"
                 :schemes [(make-scheme                            
                            :name "Birds Fly"
                            :conclusions ['(flies ?x)]
                            :premises ['(bird ?x)]
                            :exceptions ['(penguin ?x)])])
               
               (make-section
                 :name "Goods"
                 :schemes [(make-scheme
                             :name "Coins"
                             :conclusions ['(movable ?x)
                                           '(money ?x)]
                             :premises ['(coins ?x)])
                           
                           (make-scheme
                             :name "Goods"
                             :conclusions ['(goods ?x)]
                             :premises ['(movable ?x)]
                             :exceptions ['(money ?x)])
                           
                           (make-scheme
                             :name "Edible Things Are Not Goods"
                             :conclusions ['(not (goods ?x))]
                             :premises ['(edible? x)])])
                           
               (make-section
                 :name "Meta Rules"
                 :schemes [(make-scheme
                             :name "Lex Posterior"
                             :conclusions ['(prior ?r2 ?r1)]
                             :premises ['(enacted ?r1 ?d1)
                                        '(enacted ?r2 ?d2)
                                        '(later ?d2 ?d1)])])
               
               (make-section 
                 :name "Eval"
                 :schemes [(make-scheme 
                             :name "Reverse"
                             :conclusions ['(rev ?x ?y)]
                             :premises ['(eval ?y (reverse ?x))])
                           
                           (make-scheme
                             :name "Taxable Income"
                             :conclusions ['(taxable-income ?x ?t)]
                             :premises ['(income ?x ?i)
                                        '(deductions ?x ?d)
                                        '(eval ?t (- ?i ?d))])
                           
                           (make-scheme 
                             :name "Phd"
                             :conclusions ['(has-phd ?x)]
                             :premises ['(title ?x ?y)
                                        '(= ?y Dr)])
                           
                           (make-scheme
                             :name "Enrolled"
                             :conclusions ['(enrolled ?x)]
                             :premises ['(status ?x ?y)
                                        '(not= ?y exempted)])])
               
               (make-section
                 :name "Cycles"
                 :schemes [(make-scheme 
                             :name "r1"
                             :conclusions ['(not (bar ?x))]
                             :premises ['(foo ?x)])
                           
                           (make-scheme
                             :name "r2"
                             :conclusions ['(not (foo ?x))]
                             :premises ['(bar ?x)])])]))
                             

(def max-goals 50)  
(def generators (list (generate-arguments-from-theory theory1)))                  
(defn engine [facts] (make-engine max-goals facts generators))
                                   
(deftest test-engine-fact
         (let [facts '((bird Tweety))
               eng (engine facts)
               query '(bird Tweety)]
           (is (succeed? eng query query))))

(deftest test-engine-variable
         (let [facts '((bird Tweety))
               eng (engine facts)
               query '(bird ?x)]
           (is (succeed? eng query '(bird Tweety)))))

(deftest test-engine-rule
         (let [facts '((coins item1))
               eng (engine facts)
               query '(money ?x)]
           (is (succeed? eng query '(money item1)))))

;(deftest test-engine-conjunction
;         (let [facts '((enacted r1 d1)
;                       (enacted r2 d2)
;                       (later d2 d1))
;               eng (engine facts)
;               query '(prior ?x ?y)]
;           (is (succeed? eng query '(prior r2 r1)))))

;(deftest test-engine-unless1
;         (let [facts '((movable item1))
;               eng (engine facts)
;               query '(goods ?x)]
;           (is (succeed? eng query '(goods item1)))))
;
;(deftest test-engine-unless2
;         (let [facts '((coins item1))
;               eng (engine facts)
;               query '(goods ?x)]
;           (is (fail? (ask eng query) '(goods item1)))))
;
;(deftest test-engine-rebuttal
;         (let [facts '((movable item1)
;                       (edible item1))
;               eng (engine facts)
;               query '(goods ?x)]
;           (is (fail? (ask eng query) '(goods item1)))))
;
;(deftest test-engine-applies
;         (let [facts '((movable item1)
;                       (edible item1)
;                       (enacted r1 d1)
;                       (enacted r2 d2)
;                       (later d2 d1))
;               eng (engine facts)
;               query '(prior r2 r1 (goods item1))]
;           (is (succeed? eng query #{query}))))

;(deftest test-engine-negative-query
;         (let [facts '((edible i1))
;               eng (engine facts)
;               query '(not (goods ?x))]
;           (is (succeed? eng query '(not (goods i1))))))

;(deftest test-engine-eval1
;         (let [facts ()
;               eng (engine facts)
;               query '(rev '(1 2 3 4) ?y)]
;           ; to do: find some way to modify eval so that the list being
;           ; reversed doesn't need to be quoted
;           (is (succeed? eng query '(rev '(1 2 3 4) (4 3 2 1))))))
;
;(deftest test-engine-eval2
;         (let [facts '((income Sam 60000)
;                       (deductions Sam 7000))
;               eng (engine facts)
;               query '(taxable-income Sam ?r)]
;           (is (succeed? eng query '(taxable-income Sam 53000)))))
;
;(deftest test-engine-equal
;         (let [facts '((title Joe Dr))
;               eng (engine facts)
;               query '(has-phd ?x)]
;           (is (succeed? eng query '(has-phd Joe)))))
;
;(deftest test-engine-not-equal
;         (let [facts '((status Lea exempted)
;                       (status Joe active))
;               eng (engine facts)
;               query '(enrolled ?x)]
;           (is (succeed? eng query '(enrolled Joe)))))
;
;(deftest test-engine-cyclic-rules
;         (let [eng (engine ())
;               query '(foo a)]
;           (is (fail? eng query query))))

(run-tests)

