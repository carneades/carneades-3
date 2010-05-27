#!r6rs

(import (rnrs)
        (carneades argument)
        (carneades argument-builtins)
        (carneades argument-diagram)
        (carneades argument-search)
        (carneades rule)
        (carneades owl)
        (carneades lkif2)
        (carneades shell)
        (carneades stream))

(define kb1 (lkif-import "impact-kb.xml" '(transitive symmetric domain range)))
(define rb1 (lkif-data-rulebase kb1))

(define cqs '())

#;(define e1 
  (make-engine* 500 ; max-nodes
                10  ; max-turns
                empty-argument-graph
                (list (generate-arguments-from-rules rb1 cqs)
                      builtins)))

(define CarneadesEngine (string->symbol "http://carneades.berlios.de/impact-licensing#CarneadesEngine"))
(define mayUseLicenseTemplate  (string->symbol "http://carneades.berlios.de/oss-licenses#mayUseLicenseTemplate"))

;generate all possible licenses for MyCode
;(show1 `(,mayUseLicenseTemplate ,CarneadesEngine ?x) e1)

(define sols (construct-arguments `(,mayUseLicenseTemplate ,CarneadesEngine ?x)
                                  500
                                  10
                                  empty-argument-graph
                                  (list (generate-arguments-from-rules rb1 cqs)
                                        builtins)))

(define ag1 (unite-solutions-with-candidates sols))
(define ag2 (unite-solutions sols))

(view ag2)

