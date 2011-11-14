;;; Copyright (c) 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.test-shell
  (:use clojure.test
        carneades.engine.shell
        carneades.engine.argument
        carneades.engine.scheme
        carneades.engine.caes))

(def theory1 
  (make-theory
    :sections 
    [(make-section 
       :schemes 
       [(make-scheme                            
          :name "Birds Fly"
          :conclusions ['(flies ?x)]
          :premises [(pm '(bird ?x))]
          :exceptions [(pm '(penguin ?x))])])
     
     (make-section
       :schemes 
       [(make-scheme
          :name "Coins"
          :conclusions ['(movable ?x)
                        '(money ?x)]
          :premises [(pm '(coins ?x))])
        
        (make-scheme
          :name "Goods"
          :conclusions ['(goods ?x)]
          :premises [(pm '(movable ?x))]
          :exceptions [(pm '(money ?x))])
        
        (make-scheme
          :name "Edible Things Are Not Goods"
          :conclusions ['(not (goods ?x))]
          :premises [(pm '(edible ?x))])])
     
     (make-section
       :schemes 
       [(make-scheme
          :name "Lex Posterior"
          :conclusions ['(prior ?r2 ?r1)]
          :premises [(pm '(enacted ?r1 ?d1))
                     (pm '(enacted ?r2 ?d2))
                     (pm '(later ?d2 ?d1))])])
     
     (make-section 
       :schemes 
       [(make-scheme 
          :name "Reverse"
          :conclusions ['(rev ?x ?y)]
          :premises [(pm '(eval ?y (reverse ?x)))])
        
        (make-scheme
          :name "Taxable Income"
          :conclusions ['(taxable-income ?x ?t)]
          :premises [(pm '(income ?x ?i))
                     (pm '(deductions ?x ?d))
                     (pm '(eval ?t (- ?i ?d)))])
        
        (make-scheme 
          :name "Phd"
          :conclusions ['(has-phd ?x)]
          :premises [(pm '(title ?x ?y))
                     (pm '(= ?y Dr))])
        
        (make-scheme
          :name "Enrolled"
          :conclusions ['(enrolled ?x)]
          :premises [(pm '(status ?x ?y))
                     (pm '(not= ?y exempted))])])
     
     (make-section
       :schemes 
       [(make-scheme 
          :name "r1"
          :conclusions ['(not (bar ?x))]
          :premises [(pm '(foo ?x))])
        
        (make-scheme
          :name "r2"
          :conclusions ['(not (foo ?x))]
          :premises [(pm '(bar ?x))])])]))
                             

(def max-goals 500)  
(def generators (list (generate-arguments-from-theory theory1)))                  

(defn ag [facts query]  ;
  "(seq-of literal) literal -> argument-graph
   construct and evaluate an argument graph"
  (argue (make-engine max-goals facts generators)
         carneades-evaluator
         query))
                                   
(deftest test-engine-fact
         (let [facts '((bird Tweety))
               query '(bird Tweety)]
           (is (in? (ag facts query) query))))

(deftest test-engine-variable
         (let [facts '((bird Tweety))
               query '(bird ?x)]
           (is (in? (ag facts query) '(bird Tweety)))))

(deftest test-engine-rule
         (let [facts '((coins item1))
               query '(money ?x)]
           (is (in? (ag facts query) '(money item1)))))

(deftest test-engine-conjunction
         (let [facts '((enacted r1 d1)
                       (enacted r2 d2)
                       (later d2 d1))
               query '(prior ?x ?y)]
           (is (in? (ag facts query) '(prior r2 r1)))))

(deftest test-engine-unless1
         (let [facts '((movable item1))
               query '(goods ?x)]
           (is (in? (ag facts query) '(goods item1)))))

(deftest test-engine-unless2
         (let [facts '((coins item1))
               query '(goods ?x)]
           (is (out? (ag facts query) '(goods item1)))))

(deftest test-engine-rebuttal
         (let [facts '((movable item1)
                       (edible item1))
               query '(goods ?x)]
           (is (out? (ag facts query) '(goods item1)))))

(deftest test-engine-applies
         (let [facts '((movable item1)
                       (edible item1)
                       (enacted r1 d1)
                       (enacted r2 d2)
                       (later d2 d1))
               query '(priority r2 r1 (goods item1))]
           (is (in? (ag facts query) query))))

(deftest test-engine-negative-query
         (let [facts '((edible i1))
               query '(not (goods ?x))]
           (is (out? (ag facts query) '(goods i1)))))

(deftest test-engine-eval1
         (let [facts ()
               query '(rev '(1 2 3 4) ?y)]
           ; to do: find some way to modify eval so that the list being
           ; reversed doesn't need to be quoted
           (is (in? (ag facts query) '(rev '(1 2 3 4) (4 3 2 1))))))

(deftest test-engine-eval2
         (let [facts '((income Sam 60000)
                       (deductions Sam 7000))
               query '(taxable-income Sam ?r)]
           (is (in? (ag facts query) '(taxable-income Sam 53000)))))

(deftest test-engine-equal
         (let [facts '((title Joe Dr))
               query '(has-phd ?x)]
           (is (in? (ag facts query) '(has-phd Joe)))))

(deftest test-engine-not-equal
         (let [facts '((status Lea exempted)
                       (status Joe active))
               query '(enrolled ?x)]
           (is (in? (ag facts query) '(enrolled Joe)))))

(deftest test-engine-cyclic-rules
           (is (out? (ag () '(foo a)) '(foo a))))

; (run-tests)

