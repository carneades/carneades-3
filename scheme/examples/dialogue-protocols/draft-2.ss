#!r6rs

(import (rnrs)
        (carneades rule)
        (carneades argument)
        (carneades argument-builtins)
        (carneades argument-search)
        (carneades shell)
        (carneades stream))


(define-record-type move
  (fields id
          player
          speech-act
          target))


; turntaking
(define unique-move
  (rulebase   
   
   (rule next-prop (next-player prop opp))
   
   (rule next-opp (next-player opp prop))
   
   ))

; target
(define unique-reply
  (rulebase
   
   (rule next-target (if (eval ?new-target (- ?old-id 1))
                         (next-target ?old-id ?new-target)))
   
   ))

(define typical-replies
  (rulebase 
   
   
   (rule claim (and (next-speech-act (claim ?x) (claim (not ?x)))
                    (next-speech-act (claim ?x) (why ?x))
                    (next-speech-act (claim ?x) (concede ?x))))
   
   (rule why (and (next-speech-act (why ?x) (since ?x ?S))
                  (next-speech-act (why ?x) (retract ?x))))
   
   (rule since (and (next-speech-act (since ?x ?S) (why ?S)))
         (next-speech-act (since ?x ?S) (concede ?S))))  
  
  ))

(define legal-moves
  (accept default-context 
          '(
            
            )))

(define (engine max-nodes max-turns critical-questions)
  (make-engine* max-nodes max-turns legal-moves
                (list builtins
                      (generate-arguments-from-rules initial-rules critical-questions)
                      (generate-arguments-from-rules unique-move critical-questions)
                      (generate-arguments-from-rules unique-reply critical-questions)
                      (generate-arguments-from-rules typical-replies critical-questions)
                      )))

(define (get-next-player m)
  (let* ((str ((engine 20 2 '()) `(next-player (move-player 

