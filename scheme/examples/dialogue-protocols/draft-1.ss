#!r6rs

(import (rnrs)
        (carneades rule)
        (carneades argument)
        (carneades argument-builtins)
        (carneades argument-search)
        (carneades shell)
        (carneades stream))



(define initial-rules
  (rulebase
   
   (rule empty-dialogue (if (empty ?d)
                            (next-move ?d 1 prop (claim ?y) 0)))
   
   (rule pred-direct (pred ?m (next-move ?m ?id ?p ?sa ?t)))
   
   (rule pred-trans (if (and (pred ?m1 ?m2)
                             (pred ?m2 ?m3))
                        (pred ?m1 ?m3)))
   
   (rule next-rule (if (and (next-player (next-move ?m ?id ?p ?sa ?t) ?np)
                            (next-target (next-move ?m ?id ?p ?sa ?t) ?nt)
                            (next-speech-act (next-move ?m ?id ?p ?sa ?t) ?nsa)
                            (eval ?nid (+ ?id 1)))
                       (next-move (next-move ?m ?id ?p ?sa ?t) ?nid ?np ?nsa ?nt)))
   
   ))

                       
; turntaking
(define unique-move
  (rulebase
   
   
   (rule next-prop (next-player (next-move ?m ?id prop ?sa ?t) opp))
   
   (rule next-opp (next-player (next-move ?m ?id opp ?sa ?t) prop))
     
   ))

; target
(define unique-reply
  (rulebase
   
   (rule next-target (next-target (next-move ?m ?id ?p ?sa ?t) ?id))
   
   ))

(define typical-replies
  (rulebase 
   
   (rule claim (and (next-speech-act (next-move ?m1 ?id2 ?p2 (claim ?y) ?2t) (concede ?y))
                    (next-speech-act (next-move ?m1 ?id2 ?p2 (claim ?y) ?2t) (claim (not ?y))) 
                    (next-speech-act (next-move ?m1 ?id2 ?p2 (claim ?y) ?2t) (why ?y))                        
                    ))
   
   (rule why (and (next-speech-act (next-move ?m1 ?id2 ?p2 (why ?y) ?2t) (since ?y ?S))
                  (next-speech-act (next-move ?m1 ?id2 ?p2 (why ?y) ?2t) (retract ?y))))
   
   (rule since (and (next-speech-act (next-move ?m1 ?id2 ?p2 ?sa2 ?2t) (why ?S))
                        (next-speech-act (next-move ?m1 ?id2 ?p2 ?sa2 ?2t) (concede ?S)))))   
   
   
   ))

(define legal-moves
  (accept default-context 
              '((empty empty-dialogue))))

(define (engine max-nodes max-turns critical-questions)
  (make-engine* max-nodes max-turns legal-moves
                (list builtins
                      (generate-arguments-from-rules initial-rules critical-questions)
                      (generate-arguments-from-rules unique-move critical-questions)
                      (generate-arguments-from-rules unique-reply critical-questions)
                      (generate-arguments-from-rules typical-replies critical-questions)
                      )))

(define (get-move-predecessor m)
  (if (pair? m)
      (cadr m)
      'none))

(define (get-move-id m)
  (if (pair? m)
      (caddr m)
      -1))

(define (get-move-player m)
  (if (pair? m)
      (cadddr m)
      'none))

(define (get-move-speech-act m)
  (if (pair? m)
      (cadr (cdddr m))
      'none))

(define (get-move-target m)
  (if (pair? m)
      (caddr (cdddr m))
      -1))

(define (print-move m)
  (display "- move -----------")
  (newline)
  (if (pair? m)
      (begin (display "id         : ")(display (get-move-id m))
             (newline) 
             (display "player     : ")(display (get-move-player m))
             (newline)
             (display "speech-act : ")(display (get-move-speech-act m))
             (newline)
             (display "target     : ")(display (get-move-target m))
             (newline)
             (display "predecessor: ")(display (get-move-predecessor m))
             (newline))
      (begin (display m)
             (newline)))
  (display "------------------")
  (newline))


(define (get-first-possible-move d)
  (let* ((str ((engine 50 1 '()) `(next-move ,d ?id ?p ?sa ?t)))
         (s (if (not (stream-null? str))
                     (stream-car str))))
    (if (and (not (stream-null? str))
             (in? (state-arguments s)
                  (state-context s)
                  `(next-move ,d ?id ?p ?sa ?t)))
        (let ((m ((context-substitutions (state-context s)) `(next-move ,d ?id ?p ?sa ?t))))
          (set! legal-moves (accept legal-moves (list m)))
          m)        
        '())))

(define (get-all-moves d)
  (let* ((str ((engine 50 1 '()) `(next-move ,d ?id ?p ?sa ?t)))
         (mvs (stream->list str)))
    (map (lambda (s) (let ((m ((context-substitutions (state-context s)) `(next-move ,d ?id ?p ?sa ?t))))
                       (set! legal-moves (accept legal-moves (list m)))
                       m))
         (filter (lambda (s) (in? (state-arguments s)
                                  (state-context s)
                                  `(next-move ,d ?id ?p ?sa ?t)))
                 mvs))))
                         

;(define m1 (get-first-possible-move 'empty-dialogue))
;(define m2 (get-first-possible-move m1))
;
;(define mvs (get-all-moves m2))
;
;(print-move m1)
;(print-move m2)
;(map print-move mvs)
        