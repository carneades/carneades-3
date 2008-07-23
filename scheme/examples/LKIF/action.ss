#!r6rs

(import (rnrs)
        (carneades dlp)
        (carneades shell)
        (carneades argument-builtins)
        (carneades lib srfi lightweight-testing))

(ontology action
          
          (define-primitive-concept action:Transaction (and action:Collaborative_Plan(and (all mereology:direct_part action:Action) (exactly 2 mereology:direct_part))))
          
          (define-primitive-concept action:Personal_Plan  action:Plan)
          
          (define-primitive-concept action:Collaborative_Plan  action:Plan)
          
          (define-primitive-concept process:Physical_Object )
          
          (define-primitive-concept action:Trade  action:Transaction)
          
          (define-concept action:Agent (some action:actor_in action:Action))
          
          (define-primitive-concept process:Change )
          
          (define-concept action:Plan (some mereology:part action:Action))
          
          (define-primitive-concept owl:Thing )
          
          (define-concept action:Action (exactly 1 action:actor))
          
          (define-concept action:Organisation (some mereology:member action:Person))
          
          (define-primitive-concept process:Process )
          
          (define-primitive-concept action:Person (and action:Agent action:Natural_Object))
          
          (define-primitive-concept action:Natural_Object  process:Physical_Object)
          
          (define-concept action:Creation (at-least 1 process:creation))
          
          (define-concept action:Artifact (some process:result_of action:Creation))
          
          (define-primitive-concept action:Reaction  action:Action)
          
          (define-primitive-concept lkif-top:Mental_Object )
          
          (define-primitive-role mereology:member)
          
          (define-primitive-role process:participant_in)
          
          (define-primitive-role process:result_of)
          
          (define-primitive-role process:participant)
          
          (define-primitive-role mereology:direct_part)
          
          (define-primitive-role process:creation)
          
          (define-primitive-role mereology:part)
          
          ;-------------------------------
          
          (define-primitive-role action:actor :domain action:Action :range action:Action :parents( process:participant))
          
          ; :domain and :range aren't in the KRSS-Syntax as described in http://dl.kr.org/krss-spec.ps
          ; but one can define the range and domain of an role with <dlprange> <dlpdomain> syntax
          ; as illustrated in the two following axioms
          
          ; <dlprange>
          (define-primitive-concept TOP (all action:actor action:Action))
          
          ; <dlpdomain>
          (define-primitive-concept TOP (all (inverse action:actor) action:Action))
          
          ;------------------------------
          
          
          (define-primitive-role action:actor_in :domain action:Agent :range action:Agent :parents( process:participant_in))
          
          )

 (ontology facts
;           (related Caroline Tom parent)
           (instance p1 action:Personal_Plan)
           (related a1 a2 action:actor)
)

(define (engine max-nodes max-turns)
  (make-engine max-nodes max-turns 
               (list (generate-arguments-from-ontologies facts '())
                     (generate-arguments-from-ontologies action '()))))

(define e1 (engine 100 10))

(check (all-acceptable? '(action:Plan ?x) e1) => #t)
(check (all-acceptable? '(action:Action a1) e1) => #t)
(check (all-acceptable? '(action:Action a2) e1) => #t)

(check-report)

; (show '(action:Plan ?x) e1)
; (show '(action:Action a1) e1)