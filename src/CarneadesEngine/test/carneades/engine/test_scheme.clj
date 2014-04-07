;; Copyright (c) 2010 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.engine.test-scheme
  (:use clojure.pprint
        (carneades.engine statement argument argument-graph shell argument theory
         aspic dublin-core argument-evaluation policy ask  argument-generator)
        carneades.maps.lacij)
  (:require [carneades.project.admin :as project]
            [carneades.maps.lacij :refer [export]]
            [carneades.database.db :as db]
            [carneades.database.argument-graph :as ag-db]
            [carneades.database.import :refer [import-from-argument-graph]]
            [midje.sweet :refer :all :exclude [facts]]))

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
          :weight 0.6
          :premises [(pm '(movable ?x))]
          :exceptions [(pm '(money ?x))])

	 (make-scheme
          :conclusion '(goods ?x)
          :weight 0.7
          :pro false
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
          :id 'poz-1584-1-1
          :header (make-metadata :title "Poz. 1584.1.1"
                                 :description {:en ""})
          :conclusion '(minimum-guarantee ?O ?G)
          :premises [(pm '(low-or-early-advance-payment ?O))
                     (pm '(scope-of-activities ?O world-wide))
                     (pm '(annual-income ?O ?I))
                     (pm '(eval ?G (let [x (float (* ?I 0.12)) ; 12%
                                         min 40000] ; 12%
                                     (if (< x min) min x))))])

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


       (make-section  ; support cycle
	:schemes
	[(make-scheme
          :id 'r1
          :conclusion '(bar ?x)
          :premises [(pm '(foo ?x))])

	(make-scheme
          :id 'r2
          :conclusion '(foo ?x)
          :premises [(pm '(bar ?x))])])]))


(def max-goals 10000)
(def generators (list (generate-arguments-from-theory theory1)))

(defn ag [facts query]  ;
  "(seq-of literal) literal -> argument-graph
   construct and evaluate an argument graph"
  (argue (make-engine max-goals facts generators)
          aspic-grounded
         query))

(fact "Facts are in."
  (let [facts '((bird Tweety)
		(bird Peppie)
		(bird Pilot)
		(bird Ozzie))
	query '(bird Tweety)]
    (expect (in? (ag facts query) query) => true)))

(fact  "Variables are unified"
  (let [facts '((bird Tweety)
		(bird Peppie)
		(bird Pilot)
		(bird Ozzie))
	query '(bird ?x)]
    (expect (in? (ag facts query) '(bird Tweety)) => true)))

(fact "Rules works."
  (let [facts '((coins item1))
	query '(money ?x)]
    (expect (in? (ag facts query) '(money item1)) => true)))

(fact "Conjunctions can be used"
  (let [facts '((enacted r1 d1)
		(enacted r2 d2)
		(later d2 d1))
	query '(prior ?x ?y)]
    (expect (in? (ag facts query) '(prior r2 r1)) => true)))

(fact "Rules with exception work."
  (let [facts '((movable item1))
	query '(goods ?x)]
    (expect (in? (ag facts query) '(goods item1)) => true)))

(fact "Rules with exception work."
  (let [facts '((coins item1))
	query '(goods ?x)]
    (expect (undecided? (ag facts query) '(goods item1)) => true)))

(fact "Rebuttals work."
  (let [facts '((movable item1)
		(edible item1))
	query '(goods ?x)
        ag1 (ag facts query)]
    (expect (out? ag1 '(goods item1)) => true)))

(fact "Negative query works."
  (let [facts '((edible i1))
        query '(not (goods ?x))]
    (expect (out? (ag facts query) '(goods i1)) => true)))

(fact "Eval works."
  (let [facts ()
	query '(rev '(1 2 3 4) ?y)]
    ;; to do: find some way to modify eval so that the list being
    ;; reversed doesn't need to be quoted
    (expect (in? (ag facts query) '(rev '(1 2 3 4) (4 3 2 1))) => true)))

(fact "Eval with some calculation works."
  (let [facts '((income Sam 60000)
		(deductions Sam 7000))
	query '(taxable-income Sam ?r)]
    (expect (in? (ag facts query) '(taxable-income Sam 53000)) => true)))

(fact "Eval with calculation works."
  (let [facts '((low-or-early-advance-payment TO)
                (scope-of-activities TO world-wide)
                (annual-income TO 350000))
        query '(minimum-guarantee ?O ?S)]
    (expect (in? (ag facts query) '(minimum-guarantee TO 42000.0)) => true)))

(fact "Equals works."
  (let [facts '((title Joe Dr))
	query '(has-phd ?x)]
    (expect (in? (ag facts query) '(has-phd Joe)) => true)))

(fact "Deontic logic example works."
  (let [facts '((permitted (drink-alcohol Tom)))
        query '(obligated (not (drink-alcohol Tom)))]
    (expect (out? (ag facts query) query) => true)))

;; Notice the difference to the previous test, where the
;; (permitted (dring-alcohol Tom)) is accepted as a
;; fact, rather than derived from an axiom, i.e. strict rule.

(fact "The deontic logic example 2 works."
  (let [facts '()
        query '(obligated (not (drink-alcohol Tom)))]
    (expect (out? (ag facts query) query) => true)))

(fact "The not equal predicate works."
  (let [facts '((status Lea exempted)
		(status Joe active))
	query '(enrolled Joe)
        ag1 (ag facts query)]
    ;; (view ag1)
    (expect (in? ag1 '(enrolled Joe)) => true)))

(fact "Cyclic rules are undecided"
  (expect (undecided? (ag () '(foo a)) '(foo a)) => true))

(fact "Transitivity works."
  (let [facts '((parent Tom Gloria)
	      (parent Tom Jack)
	      (parent Gloria Ruth)
	      (parent Gloria Harold)
	      (parent Jack Frederick)
	      (parent Jack Elsie)),
	query '(ancestor ?x ?y)]
    (expect (in? (ag facts query) '(ancestor Tom Elsie)) => true)))

(fact "There is no regression wrt. the construction of arguments"
  (let [copyright-theory (project/load-theory "copyright" "copyright_policies")
        ag (make-argument-graph)
        ag (accept ag '((type-of-use (the-use P W) non-commercial)
                        (search-type (the-search P W) standard)))
        ag (reject ag '((type-of-use (the-use P W) commercial)
                        (search-type (the-search P W) professional)
                        (announcement (the-search P W))))
        engine (make-engine ag 50 #{} (list (generate-arguments-from-theory copyright-theory)))
        query '(may-publish ?Person ?Work)
        ag (argue engine query)
        ag (evaluate aspic-grounded ag)]

    (expect (count (arguments ag)) => 2)))

;; TODO: adapt to the new project structure
;; (deftest test-argumentconstruction-blocked
;;   (let [tour-operator-insurance (project/load-theory "xyz" "tour-operator-insurance")
;;         ag (make-argument-graph)
;;         fake-argument-from-user
;;         (reify ArgumentGenerator
;;           (generate
;;             [this goal s]
;;             (case (literal-predicate goal)
;;               scope-of-activities
;;               (build-answer s goal '[(scope-of-activities TO world-wide)])

;;               advance-payment-percent
;;               (build-answer s goal '[(advanced-payment-percent TO 20)])

;;               advance-payment-time
;;               (build-answer s goal '[(advance-payment-time TO 30)])

;;               annual-income
;;               (build-answer s goal '[(annual-income TO 350000)])

;;               ())))
;;         engine (make-engine ag 50 #{}
;;                             (list fake-argument-from-user
;;                                   (generate-arguments-from-theory tour-operator-insurance)))
;;         query '(minimum-guarantee ?O ?G)
;;         ag (argue engine query)]
;;     (is (= 4 (count (arguments ag))))))

(fact "Construction argument from an ontology works."
  (let [copyright-theory (project/load-theory "copyright" "copyright_policies")
        ag (make-argument-graph)
        fake-argument-from-user
        (reify ArgumentGenerator
          (generate
            [this goal s]
            (let [p (literal-predicate goal)]
              (cond (= p 'type-of-use)
                    (build-answer s goal '[(type-of-use (the-use P W) non-commercial)])

                    (= p 'search-type)
                    (build-answer s goal '[(search-type (the-search P W) standard)])

                    :else ()))))
        engine (make-engine ag 50 #{} (list fake-argument-from-user
                                            (generate-arguments-from-theory copyright-theory)))
        query '(may-publish ?Person ?Work)
        ag (argue engine query)]
    (expect (count (arguments ag)) => 1)))

;; TODO: adapt to the new project structure
;; (deftest test-goal-missing
;;   (let [theory (get policies 'tour-operator-insurance)
;;         ag (make-argument-graph)
;;         ag (accept ag
;;                    '[(annual-income agency 150000)
;;                      (low-or-early-advance-payment agency)
;;                      (scope-of-activities agency European)
;;                      ])
;;         engine (make-engine ag max-goals #{} (list (generate-arguments-from-theory theory)))
;;         query '(minimum-guarantee ?O ?G)
;;         ag (argue engine query)
;;         minima (filter #(= (literal-predicate %) (literal-predicate query)) (atomic-statements ag))]
;;     ;; (pprint ag)
;;     ;; (prn "minima =" minima)
;;     (is (not-empty minima))))

;; (run-tests)

(defn -main []
  (let [facts '((enacted r1 d1)
		(enacted r2 d2)
		(later d2 d1))
	query '(prior ?x ?y)]
    (in? (ag facts query) '(prior r2 r1))))
