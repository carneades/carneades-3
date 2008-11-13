#!r6rs

(library
 (carneades dialogue)
 
 (export)
 
 (import (rnrs))
 
 (define id-counter 0)
 (define (new-id)
   (set! id-counter (+ id-counter 1))
   id-counter) 
 (define (init-id)
   (set! id-counter 0))
 
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
 
 ; Tg: Mf x A -> Pow(N)
 ; target function
 (define (tg-unique-reply d p)
   (if (null? d)
       (list 0)
       (let* ((all-counter-moves (map move-id (filter (lambda (m)
                                                        (not (eq? (move-player m) p)))
                                                      d)))
              (already-replied (map move-target d)))
         (filter (lambda (m)
                   (not (member m already-replied)))
                 all-counter-moves))))
         
 
 (define (tg-multiple-reply d p)
   (if (null? d)
       (list 0)
       (map move-id (filter (lambda (m)
                              (not (eq? (move-player m) p)))
                            d))))
 
 
 ; choose-functions
 
 (define (choose-next-player d t)
   (let ((possible-players (t d)))
     (display "choose next player: ")
     (display possible-players)
     (let ((np (read)))
       (if (member np possible-players)
           np
           (begin (display "wrong player: ")
                  (display np)
                  (newline)
                  (choose-next-player d t))))))
 
 (define (choose-next-target d tg np)
   (let ((possible-targets (tg d np)))
     (if (null? possible-targets)
         (begin (display "sorry, no possible targets for player ")
                (display np)
                (newline)
                (display "please choose another player")
                (newline)
                -1)
         (begin (display "choose next target: ")
                (display possible-targets)
                (let ((nt (read)))
                  (if (member nt possible-targets)
                      nt
                      (begin (display "wrong target: ")
                             (display nt)
                             (newline)
                             (choose-next-target d tg np))))))))
 
 (define (choose-next-reply d r nt)
   (let* ((sa (if (null? d)
                  'none
                  (move-speech-act (list-ref d (- (length d) nt)))))
          (possible-replies (r sa)))
     (if (null? possible-replies)
         (begin (display "sorry, no possible replies to ")
                (display sa)
                (newline)
                (display "please choose another target")
                (newline)
                '())
         (begin (display "choose next reply: ")
                (display possible-replies)
                (let ((nsa (read)))
                  (if (member nsa possible-replies)
                      nsa
                      (begin (display "wrong reply: ")
                             (display nsa)
                             (newline)
                             (choose-next-reply d r nt))))))))
 
 ; choose-next-move:
 ; Mf x (Mf -> Pow(A)) x (Mf -> Pow(N)) x (Lc -> Pow(Lc)) -> Mf
 (define (choose-next-move d t tg r)
   (if (null? d)
       (init-id))
   (let* ((np (choose-next-player d t))
          (nt (choose-next-target d tg np))
          (nsa (choose-next-reply d r nt)))
     (cons (make-move (new-id)
                      np
                      nsa
                      nt)
           d)))
 
 (define mv1 (choose-next-move '() t-unique-move tg-unique-reply r-standard))

 
 )