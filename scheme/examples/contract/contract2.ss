#!r6rs

(import (rnrs)
        (carneades lkif2)
        (carneades argument-diagram))

(define import-data (lkif-import "contract2.xml"))

(define stages (lkif-data-stages import-data))

(define st1 (car stages))

(view (stage-argument-graph st1) (stage-context st1))
