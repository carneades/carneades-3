#!r6rs

(import (rnrs)
        (carneades lkif2)
        (carneades argument-diagram))

(define ldata (lkif-import "Tweety.xml"))

(define stages (lkif-data-stages ldata))

(define st1 (car stages))

(view (stage-argument-graph st1) (stage-context st1))
