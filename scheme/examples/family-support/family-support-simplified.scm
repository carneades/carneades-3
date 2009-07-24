#!r6rs

(import (rnrs)
        (carneades owl)
        (carneades lkif2)
        (carneades argument)
        (carneades argument-builtins)
        (carneades dlp) ; Description Logic Programming subset of Description Logic
        (carneades rule)
        (carneades shell))

;(ontology family-relations
;          
;          (define-primitive-role family:ancestor family:relative)
;          (define-primitive-role family:parent family:ancestor)
;          (define-primitive-role family:ancestor (inverse family:descendent))
;          (define-primitive-role family:mother family:parent)
;          (define-primitive-role family:father family:parent)
;          (define-primitive-role family:sibling family:relative)
;          (define-primitive-role family:brother family:sibling)
;          (define-primitive-role family:sister family:sibling))

(define family-relations (owl-import "./family-support.owl"))

;(define family-data (lkif-import "family-support.xml"))
;(define family-support (lkif-data-rulebase family-data))

; type critical-question = excluded | priority | valid

(define argument-generators
  (list builtins
        ; (generate-arguments-from-ontology family-relations '())
        (generate-arguments-from-rules family-relations '())
      ;  (generate-arguments-from-rules family-support '(excluded))
  ))

(define ag1 (accept empty-argument-graph 
                    `(
                      (,(string->symbol "http://www.semanticweb.org/ontologies/2009/6/family-support.owl#father") Dustin Tom)
                      )))

; 1. Find the best arguments about whether Tom is obligated to support Gloria, without considering the
; issue of undue hardship

(define e1 (make-engine* 50 2 ag1 argument-generators))

; (diagram1 '(family:obligatedToSupport Max ?y) e1)

; end of file
