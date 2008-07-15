#!r6rs

(import (rnrs base)
        (carneades shell)
        (carneades argument-builtins)
        (carneades rule)
        (carneades lib srfi lightweight-testing))

(define null '())

(define rb1 
  (rulebase
   
   (rule r1 
         (if (and (movable ?c)
                  (unless (money ?c)))
             (goods ?c)))
   
   (rule r3 (if (coins ?x) (money ?x)))
   
   
   ; r4 and r5 are to test multiple conclusions
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
   
   ; to test exclusionary rules
   (rule r14 (if (bird ?x) (flies ?x)))
   (rule* r15 (if (penguin ?x) (excluded r14 (flies ?x))))
   
   
   ; to test negative exceptions, by asking the validity critical question.
   (rule repeal
         (if (repealed ?r)
             (not (valid ?r))))
   
   ; disjunction tests
   (rule r19 
         (if (or (p1 ?x) 
                 (p2 ?x)) 
             (p3 ?x)))
   
   (rule r20 
         (if (or (and (p4 ?x) (p5 ?x))
                 (and (p6 ?x) (p7 ?x))
                 (p8 ?x))
             (p9 ?x)))
   
   ; eval test
   (rule r21 
         (if (and (p10 ?x) (eval ?z (reverse ?x))) 
             (p11 ?z)))
   
   
   ; another eval test, to demonstrate calculations
   (rule r22 
         (if (and (income ?x ?i)
                  (deductions ?x ?d)
                  (eval ?t (- ?i ?d)))
             (taxable-income ?x ?t)))
   
   (rule* facts 
          (coins item1)
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
          (deductions Sam 7000))
   
   )) ; end of rule base

; type question = excluded | priority | valid

; engine integer integer (list-of symbol) -> statement -> (stream-of argument-state)
(define (engine max-nodes max-turns critical-questions)
  (make-engine max-nodes max-turns 
               (list (generate-arguments-from-rules rb1 critical-questions) builtins)))

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

; Example commands
; (ask '(goods item2) (engine 20 2 null))
; (show '(goods item2) (engine 20 2 '(priority)))


