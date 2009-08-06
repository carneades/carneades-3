; PS/Tk Example Program Tic Tac Toe
; Copyright (C) 2006 Nils M Holm
; See the PS/Tk license for conditions of use.

#!r6rs

(import (rnrs)
        (carneades lib pstk))

(define Font "Courier 30")

(define Field (vector '_ '_ '_
                      '_ '_ '_
                      '_ '_ '_))

(define Buttons #f)
(define Qbutton #f)

(define Lost #f)

(define Moves '(((_ o o)  (o o o)) 
                ((o _ o)  (o o o)) 
                ((o o _)  (o o o)) 
                ((_ x x)  (o x x)) 
                ((x _ x)  (x o x)) 
                ((x x _)  (x x o)) 
                ((_ x _)  (o x _)) 
                ((_ _ x)  (_ o x)) 
                ((x _ _)  (x o _)) 
                ((_ x o)  (o x o)) 
                ((_ o x)  (o o x)) 
                ((x _ o)  (x o o)) 
                ((x o _)  (x o o)) 
                ((o _ x)  (o o x)) 
                ((o x _)  (o x o)) 
                ((_ _ o)  (o _ o)) 
                ((_ o _)  (o o _)) 
                ((o _ _)  (o _ o)) 
                ((_ _ _)  (_ o _))))

(define pattern car)
(define subst cadr)

(define (match-row f1 f2 f3 rule)
  (cond ((and (eq? (vector-ref Field f1) (car (pattern rule)))
              (eq? (vector-ref Field f2) (cadr (pattern rule)))
              (eq? (vector-ref Field f3) (caddr (pattern rule))))
      (let ((new (subst rule)))
        (vector-set! Field f1 (car new))
        (vector-set! Field f2 (cadr new))
        (vector-set! Field f3 (caddr new))
        new))
    (else #f)))

(define (try-moves rules)
  (cond ((null? rules) '())
    (else (let ((done (or (match-row 0 4 8 (car rules))
                          (match-row 2 4 6 (car rules))
                          (match-row 0 1 2 (car rules))
                          (match-row 3 4 5 (car rules))
                          (match-row 6 7 8 (car rules))
                          (match-row 0 3 6 (car rules))
                          (match-row 1 4 7 (car rules))
                          (match-row 2 5 8 (car rules)))))
            (cond (done
                (cond ((equal? done '(o o o))
                    (set! Lost #t)
                    (Qbutton 'configure 'text: "Oops!"
                             'fg: "#ff0000"))))
              (else (try-moves (cdr rules))))))))

(define (paint-field)
  (map (lambda (b p)
         (cond ((eq? p 'o)
             (b 'configure 'text: "O" 'fg: 'blue
                           'activeforeground: 'blue))
           ((eq? p 'x)
             (b 'configure 'text: "X" 'fg: 'red
                           'activeforeground: 'red))))
       Buttons (vector->list Field)))

(define (move new)
  (cond ((and (not Lost)
              (eq? '_ (vector-ref Field new)))
      (vector-set! Field new 'x)
      (cond ((eq? '_ (vector-ref Field 4))
          (vector-set! Field 4 'o))
        (else (try-moves Moves)))
      (paint-field)))
  '())

(define tk (tk-start))
(tk/wm 'title tk "Tic Tac Toe")

(let* ((field
         (lambda (parent n)
           (parent 'create-widget 'button
                   'text: n 'font: Font
                   'fg: "#a0a0a0"
                   'bg: "#e0e0e0"
                   'command: (lambda () (move (- n 1))))))
       (row
         (lambda (n)
           (let* ((f (tk 'create-widget 'frame))
                  (b1 (field f n))
                  (b2 (field f (+ 1 n)))
                  (b3 (field f (+ 2 n))))
             (tk/pack b1 b2 b3 'side: 'left)
             (list f b1 b2 b3))))
       (r1 (row 1))
       (r2 (row 4))
       (r3 (row 7)))
  (tk/pack (car r1) (car r2) (car r3) 'side: 'top)
  (set! Buttons (append (cdr r1) (cdr r2) (cdr r3)))
  (set! Qbutton (tk 'create-widget 'button
                    'text: "Quit"
                    'command: tk-end))
  (tk/pack Qbutton 'side: 'top 'expand: #t 'fill: 'x)
  (tk-event-loop tk))
