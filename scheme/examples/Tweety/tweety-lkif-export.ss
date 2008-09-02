#!r6rs

(import (rnrs)
        (carneades lkif2)
        (carneades argument-diagram))

(define ldata (lkif-import "Tweety.xml"))

(lkif-export ldata "Tweety-export.xml")

(set! ldata (lkif-import "Tweety-export.xml"))

(define stages (lkif-data-stages ldata))

(define st1 (car stages))

(view (stage-argument-graph st1) (stage-context st1))