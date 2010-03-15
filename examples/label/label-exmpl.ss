#!r6rs

(import (rnrs)
        (carneades argument)
        (carneades argument-diagram)
        (carneades label)
        (carneades statement)
        (carneades lkif2))

(define d (lkif-import "label-test.lkif"))

(define ag (car (lkif-data-argument-graphs d)))

(define asm '((not "V") (not "T") "W" "S" ))

(define in-label (statement-in-label ag asm "P"))
(define out-label (statement-out-label ag asm "P"))
(define c-in-label (statement-in-label ag asm '(not "P")))
(define c-out-label (statement-out-label ag asm '(not "P")))

(display "in(+P): ")
(display in-label)
(newline)

(display "out(+P): ")
(display out-label)
(newline)

(display "in(-P): ")
(display c-in-label)
(newline)

(display "out(-P): ")
(display c-out-label)
(newline)

(view (accept ag asm))