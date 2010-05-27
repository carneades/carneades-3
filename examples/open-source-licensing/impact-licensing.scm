#!r6rs

(import (rnrs)
        (carneades argument)
        (carneades argument-builtins)
        (carneades rule)
        (carneades owl)
        (carneades lkif2)
        (carneades shell))

(define kb1 (lkif-import "impact-kb.xml" '(transitive symmetric domain range)))
(define rb1 (lkif-data-rulebase kb1))

(define cqs '())

(define e1 
  (make-engine* 500 ; max-nodes
                10  ; max-turns
                empty-argument-graph
                (list (generate-arguments-from-rules kb1 cqs)
                      builtins)))

(define CarneadesEngine (string->symbol "http://carneades.berlios.de/impact-licensing.owl#CarneadesEngine"))
(define mayUseLicenseTemplate  (string->symbol "http://carenades.berlios.de/oss-licenses.owl#mayUseLicenseTemplate"))

;generate all possible licenses for MyCode
(show1 `(,mayUseLicenseTemplate ,CarneadesEngine ?x) e1)

