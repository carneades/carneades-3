#!r6rs

(import (rnrs)
        (carneades rule)
        (carneades shell)
        (carneades argument-builtins)
        (carneades lkif2)
        (carneades argument-diagram)
        (carneades unify)
        (carneades lib srfi lightweight-testing)
        (carneades lib srfi format))

(define null '())

(define i (lkif-import "lkif-test.xml"))

(define sources (lkif-data-sources i))

(define axioms (lkif-data-context i))

(define rb1 (lkif-data-rulebase i))

(define stages (lkif-data-stages i))

(define stage1 (car stages))

;(view (stage-argument-graph stage1) (stage-context stage1))

(define d1 (call-with-values open-string-output-port 
                             (lambda (p e)
                               (diagram* (stage-argument-graph stage1)
                                         (stage-context stage1)
                                         identity 
                                         (lambda (s) (format "~a" s)) p)
                               (e))))

(define d2 "digraph g {\n    rankdir = \"RL\";\n    g1 [shape=box, label=\"Tweety is a penguin.\", style=\"filled\"];\n    g2 [shape=box, label=\"Birds normally fly.\", style=\"\"];\n    g3 [shape=box, label=\"Tweety is an abnormal bird.\", style=\"filled\"];\n    g4 [shape=box, label=\"Tweety is a bird.\", style=\"filled\"];\n    g5 [shape=box, label=\"Tweety can fly.\", style=\"\"];\n    g6 [shape=ellipse, label=\"a1\", style=\"\"];\n    g6 -> g5;\n    g4 -> g6 [arrowhead=\"none\"];\n    g2 -> g6 [arrowhead=\"dot\"];\n    g3 -> g6 [arrowhead=\"odot\"];\n    g7 [shape=ellipse, label=\"a2\", style=\"filled\"];\n    g7 -> g3;\n    g1 -> g7 [arrowhead=\"none\"];\n}\n")

(define (engine max-nodes max-turns critical-questions)
  (make-engine* max-nodes max-turns
                axioms
                (list (generate-arguments-from-rules rb1 critical-questions) builtins)))

(define lkif-engine1 (engine 20 2 '()))
(define lkif-engine2 (engine 20 2 '(excluded)))

(display (string=? d1 d2))
(newline)
(display (all-in? '(flies ?bird) lkif-engine1))
(newline)
(display (not (some-in? '(flies ?bird) lkif-engine2)))




