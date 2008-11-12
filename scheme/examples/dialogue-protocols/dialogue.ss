#!r6rs

(library
 (carneades dialogue)
 
 (export)
 
 (import (rnrs)
         (carneades argument)
         (carneades rule))
 
 (define id-counter 0)
 (define (new-id)
   (set! id-counter (+ id-counter 1))
   id-counter) 
 ; topic language Lt
 
 ; communication language Lc = {claim, why, concede, retract, since}
  
 ; participants A = {prop, opp}
 
 ; context K subset of Lt; as known in carneades
 
 ; moves M = N x A x Ltc x N
 ; id x player x speech-act x target
 (define-record-type move
   (fields id
           player
           speech-act
           target))
 
 ; infinite dialogues Mi {d = m1, ..., mn, ... | mi in M; id(mi)=i; target(m1)=0; target(mi) < i}
 ; finite dialogues Mf
 
 (define empty-dialogue '()) 
 
 ; P: D -> Pow(M)
 ; D subset of Mf, where:
 ;    d in D and P(d)=m <=> (d,m) in D
 ; legal finite dialogues
 
 ; T: Mf -> Pow(A)
 ; turntaking function
 (define (t-unique-move d)
   (if (null? d)
       (list 'prop)
       (if (eq? (move-player (car d)) 'prop)
           (list 'opp)
           (list 'prop))))
 
 (define (t-multiple-move d)
   (if (null? d)
       (list 'prop)
       (if (null? (cdr d))
           (list 'opp)
           (list 'prop 'opp))))
 
 ; R: Lc -> Pow(Lc)
 ; reply function
 (define (r-standard l)
   (case l
     ((claim) (list 'why 'concede))
     ((why) (list 'since 'retract))
     ((since) (list 'why 'concede))
     ((none) (list 'claim 'since))
     (else '())))
 
 ; Tg: Mf -> Pow(N)
 ; target function
 (define (tg-unique-reply d)
   (if (null? d)
       (list 0)
       (list (move-id (car d)))))
 
 (define (tg-multiple-reply d)
   (if (null? d)
       (list 0)
       (map move-id d)))
 
 ; choose-next-move:
 ; Mf x (Mf -> Pow(A)) x (Mf -> Pow(N)) x (Lc -> Pow(Lc)) -> Mf
 (define (choose-next-move d t tg r)
   (display "Choose next player: ")
   (display (t d))
   (let ((np (read)))
     (if (member np (t d))
         (begin (display "Choose next target: ")
                (display (tg d))
                (let ((nt (read)))
                  (if (member nt (tg d))
                      (let ((sa (if (null? d)
                                    'none
                                    (move-speech-act (list-ref d (- (length d) nt))))))
                        (display "Choose next reply: ")
                        (display (r sa))
                        (let ((nsa (read)))
                          (if (member nsa  (r sa))
                              (cons (make-move (new-id)
                                               np
                                               nsa
                                               nt)
                                    d)
                              (begin (display "Wrong reply: ")
                                     (write nsa)
                                     (newline)
                                     d))))
                      (begin (display "Wrong target: ")
                             (write nt)
                             (newline)
                             d))))
         (begin (display "Wrong player: ")
                (write np)
                (newline)
                d))))
 
 (define mv1 (choose-next-move '() t-unique-move tg-unique-reply r-standard))

 
 )