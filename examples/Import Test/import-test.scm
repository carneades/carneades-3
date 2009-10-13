#!r6rs

(import (rnrs)
        (carneades argument)
        (carneades argument-builtins)
        (carneades rule)
        (carneades owl)
        (carneades lkif2)
        (carneades shell))


(define test1 "test1.xml")

(define circle1 "circle1.lkif")

(define t1 (lkif-import test1 '(transitive symmetric domain range)))
(define c1 (lkif-import circle1 '()))

(define rb1 (lkif-data-rulebase t1))

(define (engine max-nodes max-turns critical-questions)
  (make-engine* max-nodes max-turns empty-argument-graph
                (list (generate-arguments-from-rules rb1 critical-questions)
                      builtins)))

(define e1 (engine 50 1 '()))

(ask `(,(string->symbol "http://www.carneades/owl1.owl#a") ?x) e1)