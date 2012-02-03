;;; Copyright (c) 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.test-scheme
  (:use clojure.test
        carneades.engine.shell
        carneades.engine.argument
        carneades.engine.scheme
        carneades.engine.caes
        carneades.engine.dublin-core
        carneades.engine.argument-evaluation
        carneades.maps.lacij))

(def theory1 
  (make-theory
    :sections 
    [(make-section 
       :schemes 
       [(make-scheme                            
          :conclusion '(flies ?x)
          :premises [(pm '(bird ?x))]
          :exceptions [(pm '(penguin ?x))])])
     
     (make-section
       :schemes 
       [(make-scheme
          :conclusion '(movable ?x)
          :premises [(pm '(coins ?x))])
        
        (make-scheme
          :conclusion '(money ?x)
          :premises [(pm '(coins ?x))])
        
        (make-scheme
          :id 'r1
          :conclusion '(goods ?x)
          :premises [(pm '(movable ?x))]
          :exceptions [(pm '(money ?x))])
        
        (make-scheme
          :conclusion '(not (goods ?x))
          :premises [(pm '(edible ?x))])])
     
     (make-section
       :schemes 
       [(make-scheme
          :conclusion '(prior ?r2 ?r1)
          :premises [(pm '(enacted ?r1 ?d1))
                     (pm '(enacted ?r2 ?d2))
                     (pm '(later ?d2 ?d1))])])
     
     (make-section 
       :schemes 
       [(make-scheme 
          :name "Reverse"
          :conclusion '(rev ?x ?y)
          :premises [(pm '(eval ?y (reverse ?x)))])
        
        (make-scheme
          :name "Taxable Income"
          :conclusion '(taxable-income ?x ?t)
          :premises [(pm '(income ?x ?i))
                     (pm '(deductions ?x ?d))
                     (pm '(eval ?t (- ?i ?d)))])
        
        (make-scheme 
          :name "Phd"
          :conclusion '(has-phd ?x)
          :premises [(pm '(title ?x ?y))
                     (pm '(= ?y Dr))])
        
        (make-scheme
          :name "Enrolled"
          :conclusion '(enrolled ?x)
          :premises [(pm '(status ?x ?y))
                     (pm '(not= ?y exempted))])])
     
     (make-section  
      :schemes
      [(axiom '(permitted (drink-alcohol ?x)))
       
       (make-scheme
        :id 'not-obligated
        :conclusion '(not (obligated (not ?P)))
        :premises    [(pm '(permitted ?P))])
       
       (make-scheme 
        :conclusion '(not (permitted ?P))
        :premises    [(pm '(obligated (not ?P)))])])
     
     (make-section
       :schemes
       [(make-scheme
          :conclusion '(ancestor ?x ?y)
          :premises [(pm '(parent ?x ?y))])
       
       (make-scheme
         :conclusion '(ancestor ?x ?y)  ; y is an ancestor of x
         :premises [(pm '(parent ?x ?z)) ; z is a parent of x
                    (pm '(ancestor ?z ?y))])])
                
     
     (make-section
       :schemes 
       [(make-scheme 
          :id 'r1
          :conclusion '(not (bar ?x))
          :premises [(pm '(foo ?x))])
        
        (make-scheme
          :id 'r2
          :conclusion '(not (foo ?x))
          :premises [(pm '(bar ?x))])])]))
                             

(def max-goals 10000)  
(def generators (list (generate-arguments-from-theory theory1)))                  

(defn ag [facts query]  ;
  "(seq-of literal) literal -> argument-graph
   construct and evaluate an argument graph"
  (argue (make-engine max-goals facts generators)
         carneades-evaluator
         query))
                                   
(deftest test-engine-facts
         (let [facts '((bird Tweety)
                       (bird Peppie)
                       (bird Pilot)
                       (bird Ozzie))
               query '(bird Tweety)]
           (is (in? (ag facts query) query))))

(deftest test-engine-variable
         (let [facts '((bird Tweety)
                       (bird Peppie)
                       (bird Pilot)
                       (bird Ozzie))
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
           (is (undecided? (ag facts query) '(goods item1)))))

(deftest test-engine-rebuttal
         (let [facts '((movable item1)
                       (edible item1))
               query '(goods ?x)]
           (is (undecided? (ag facts query) '(goods item1)))))

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

(deftest test-deontic-logic1
         (let [facts '((permitted (drink-alcohol Tom)))
               query '(obligated (not (drink-alcohol Tom)))]
           (is (out? (ag facts query) query))))

;; To Do: confirm that the semantics are right in the following
;; test.  Should the query by out instead of undecided?
;; Notice the difference to the previous test, where the
;; same query is out, when (permitted (dring-alcohol Tom)) is
;; in becuase it is a fact, rather than because it can be
;; derived from an axiom.

(deftest test-deontic-logic2
         (let [facts '()
               query '(obligated (not (drink-alcohol Tom)))]
           (is (undecided? (ag facts query) query))))

(deftest test-engine-not-equal
         (let [facts '((status Lea exempted)
                       (status Joe active))
               query '(enrolled Joe)]
           (is (in? (ag facts query) '(enrolled Joe)))))

(deftest test-engine-cyclic-rules
           (is (undecided? (ag () '(foo a)) '(foo a))))

(deftest test-transitivity
         (let [facts '((parent Tom Gloria)
                       (parent Tom Jack)
                       (parent Gloria Ruth)
                       (parent Gloria Harold)
                       (parent Jack Frederick)
                       (parent Jack Elsie)),
               query '(ancestor ?x ?y)]
           (is (in? (ag facts query) '(ancestor Tom Elsie)))))

; (run-tests)

(defn -main []
    (let [facts '((enacted r1 d1)
                  (enacted r2 d2)
                  (later d2 d1))
          query '(prior ?x ?y)]
       (in? (ag facts query) '(prior r2 r1))))
