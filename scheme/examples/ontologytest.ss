#!r6rs

(import (rnrs) 
        (carneades dlp)
        (carneades shell)
        (carneades argument-builtins)
        (carneades lib srfi lightweight-testing)
        (carneades argument-diagram) ; debug
        )


(ontology kb1
          
          (related Caroline Tom parent)
          (related Caroline Ines parent)
          (related Dustin Tom parent)
          (related Dustin Ines parent)
          (related Tom Gloria parent)
          (related Ines Hildegard parent)
          (instance Tom Male)
          (instance Tom Parent)
          (instance Ines Female)
          (instance Ines Parent)
          
          (define-primitive-role parent ancestor)
          
          (define-concept Father (and Male Parent))
          
          (define-concept Mother (and Female Parent))
          
          (define-primitive-role (transitive-closure ancestor) ancestor)
          
          (define-primitive-concept Human (or Male Female))
          
          (define-concept (or (and Child Male) Female) (not LiableToMilitaryService)))


;(define kb2 (add-ontologies kb1 (list (axiom fact9 (instance Ines Female))
;                                        (axiom fact10 (instance Ines Parent)))))

;(define kb3 (add-ontologies kb2 (list (axiom o3 (define-concept Mother (and Female Parent))))))


(define (engine max-nodes max-turns)
  (make-engine max-nodes max-turns 
               (list (generate-arguments-from-ontology kb1 '()))))

(define e1 (engine 100 10))

(check (all-in? '(parent ?x ?y) e1) => #t)
(check (all-in? '(ancestor ?x ?y) e1) => #t)
(check (all-in? '(ancestor Caroline ?y) e1) => #t)
(check (all-in? '(ancestor Caroline Tom) e1) => #t)
(check (no-argument-found? '(parent Hildegard Tom) e1) => #t)
(check (all-in? '(ancestor Caroline Gloria) e1) => #t)
(check (all-in? '(applies ?r (parent ?x ?y)) e1) => #t)
(check (all-in? '(Father Tom) e1) => #t)
(check (all-in? '(Mother Ines) e1) => #t)

(check-report)

; Example commands
; (ask '(goods item2) e1)
; (show '(goods item2) e1)


