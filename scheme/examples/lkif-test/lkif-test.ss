#!r6rs

(import (rnrs)
        (carneades rule)
        (carneades shell)
        (carneades argument-builtins)
        (carneades lkif2)
        (carneades argument-diagram)
        (carneades lib srfi lightweight-testing))

(define null '())

(define i (lkif-import "lkif-test.xml"))

(define sources (lkif-data-sources i))

(define axioms (lkif-data-context i))

(define rb (lkif-data-rulebase i))

(define stages (lkif-data-stages i))

(define stage1 (car stages))

;(view (stage-argument-graph stage1) (stage-context stage1))

(define (engine max-nodes max-turns critical-questions)
  (make-engine max-nodes max-turns 
               (list (generate-arguments-from-rules rb critical-questions) builtins)))


(check (all-acceptable? '(bird Tweety) (engine 20 1 null)) => #t)
(check (all-acceptable? '(bird ?x) (engine 20 1 null)) => #t)
(check (all-acceptable? '(money item1) (engine 20 1 null)) => #t) ; coins are money
(check (all-acceptable? '(prior ?r1 ?r2) (engine 20 1 null)) => #t)
(check (all-acceptable? '(p3 a) (engine 20 1 null)) => #t) ; disjunction of atomic statements
(check (all-acceptable? '(p9 a) (engine 20 1 null)) => #t) ; disjunction of conjunctions
(check (all-acceptable? '(goods item1) (engine 20 1 null)) => #t) ; find pro argument
(check (not (all-acceptable? '(goods item1) (engine 20 2 null))) => #t) ; unless money exception
(check (not (all-acceptable? '(goods item2) (engine 20 2 null))) => #t) ; edible things are not goods
(check (not (all-acceptable? '(convenient item1) (engine 20 2 '(valid)))) => #t) ; repealed rules are not valid
(check (not (all-acceptable? '(goods item2) (engine 20 2 '(priority)))) => #t) ; lex posterior
; to do: fix the following test. The success predicate tests only whether one is found, not all  
; (test-true "multiple rule conclusions" (all-acceptable? '(convenient ?x)) (engine 20 1 null))
(check (all-acceptable? '(not (goods item2)) (engine 20 3 null)) => #t)
(check (not (all-acceptable? '(flies Tweety) (engine 20 2 '(excluded)))) => #t)
(check (all-acceptable? '(applies ?r (goods ?x)) (engine 20 1 null)) => #t)
; to do: test negative conditions and exceptions
; to do: test rules with negative conclusions
(check (all-acceptable? '(p11 ?x) (engine 20 1 null)) => #t) ; reverse a list
(check (all-acceptable? '(taxable-income Sam ?x) (engine 20 1 null)) => #t) ; calculations
; to do: test assumptions -- a statement is questioned by making an argument pro or con the statement
; to do: event calculus tests
(check-report)