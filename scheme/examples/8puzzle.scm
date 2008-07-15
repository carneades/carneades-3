
#!r6rs 

(import (rnrs)
        (rnrs records syntactic)
        (carneades search)
        (carneades lib match)
        (carneades stream))

(define (make-state r1c1 r1c2 r1c3
                    r2c1 r2c2 r2c3
                    r3c1 r3c2 r3c3)
  (list r1c1 r1c2 r1c3
        r2c1 r2c2 r2c3
        r3c1 r3c2 r3c3))

(define (state-r1c1 s) (list-ref s 0))
(define (state-r1c2 s) (list-ref s 1))
(define (state-r1c3 s) (list-ref s 2))
(define (state-r2c1 s) (list-ref s 3))
(define (state-r2c2 s) (list-ref s 4))
(define (state-r2c3 s) (list-ref s 5))
(define (state-r3c1 s) (list-ref s 6))
(define (state-r3c2 s) (list-ref s 7))
(define (state-r3c3 s) (list-ref s 8))

(define (state->datum s) s)

(define (move? x) (member x '(up down right left)))

; type label = move

; goal?: state -> boolean
(define (goal? s)
  (match s 
    ((1 2 3 
      8 0 4
      7 6 5) #t)
    (_ #f)))

; move: move node -> node | #f
(define (move m p)
  (let ((depth (+ (node-depth p) 1)))
    (case m
      ((up) 
       (match (node-state p)
         ((a b c 0 d e f g h) 
          (make-node depth 'up p (make-state 0 b c a d e f g h)))
         ((a b c d 0 e f g h)
          (make-node depth 'up p (make-state a 0 c d b e f g h)))
         ((a b c d e 0 f g h)
          (make-node depth 'up p (make-state a b 0 d e c f g h)))
         ((a b c d e f 0 g h)
          (make-node depth 'up p (make-state a b c 0 e f d g h)))
         ((a b c d e f g 0 h)
          (make-node depth 'up p (make-state a b c d 0 f g e h)))
         ((a b c d e f g h 0)
          (make-node depth 'up p (make-state a b c d e 0 g h f)))
         (_ #f)))
      ((down) 
       (match (node-state p)
         ((0 b c d e f g h i)
          (make-node depth 'down p (make-state d b c 0 e f g h i)))
         ((a 0 c d e f g h i)
          (make-node depth 'down p (make-state a e c d 0 f g h i)))
         ((a b 0 d e f g h i)
          (make-node depth 'down p (make-state a b f d e 0 g h i)))
         ((a b c 0 e f g h i)
          (make-node depth 'down p (make-state a b c g e f 0 h i)))
         ((a b c d 0 f g h i)
          (make-node depth 'down p (make-state a b c d h f g 0 i)))
         ((a b c d e 0 g h i)
          (make-node depth 'down p (make-state a b c d e i g h 0)))
         (_ #f)))
      ((right) 
       (match (node-state p)
         ((0 b c d e f g h i)
          (make-node depth 'right p (make-state b 0 c d e f g h i)))
         ((a b c 0 e f g h i)
          (make-node depth 'right p (make-state a b c e 0 f g h i)))
         ((a b c d e f 0 h i)
          (make-node depth 'right p (make-state a b c d e f h 0 i)))
         ((a 0 c d e f g h i)
          (make-node depth 'right p (make-state a c 0 d e f g h i)))
         ((a b c d 0 f g h i)
          (make-node depth 'right p (make-state a b c d f 0 g h i)))
         ((a b c d e f g 0 i)
          (make-node depth 'right p (make-state a b c d e f g i 0)))
         (_ #f)))
      ((left) 
       (match (node-state p)
         ((a 0 c d e f g h i)
          (make-node depth 'left p (make-state 0 a c d e f g h i)))
         ((a b c d 0 f g h i)
          (make-node depth 'left p (make-state a b c 0 d f g h i)))
         ((a b c d e f g 0 i)
          (make-node depth 'left p (make-state a b c d e f 0 g i)))
         ((a b 0 d e f g h i)
          (make-node depth 'left p (make-state a 0 b d e f g h i)))
         ((a b c d e 0 g h i)
          (make-node depth 'left p (make-state a b c d 0 e g h i)))
         ((a b c d e f g h 0)
          (make-node depth 'left p (make-state a b c d e f g 0 h)))
         (_ #f)))
      (else #f))))

; moves: node -> (stream-of node)
(define (moves n)
  (define (f m) 
    (let ((r (move m n)))
      (if r (list r) '())))
  (list->stream (apply append (map f '(up down right left)))))

; pr0: trivial problem; base case; 0 moves required; the starting state satisfies the goal
(define pr0
  (make-problem (make-root (make-state 1 2 3 8 0 4 7 6 5))
                moves
                goal?))

; pr1: just 1 move required
(define pr1
  (make-problem (make-root (make-state 1 2 3 8 4 0 7 6 5))
                moves
                goal?))

; pr5: 5 moves required
(define pr5
  (make-problem (make-root (make-state 2 8 3 1 6 4 7 0 5))
                moves
                goal?))

; show: (stream-of node) -> void
(define (show str) 
  (if (stream-null? str)
      '() 
      (begin (display (path (stream-car str)))
             (newline))))

; (define sols1 (search pr5 (depth-first 100)))
(show (search pr5 (breadth-first (make-resource 1000))))
;  (set! n 0)
(show (search pr5 ((iterative-deepening 10 10) (make-resource 200))))


