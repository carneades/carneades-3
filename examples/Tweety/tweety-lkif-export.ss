#!r6rs

(import (rnrs)
        (carneades lkif2)
        (carneades argument-diagram))

(define import-data (lkif-import "Tweety.xml"))

(lkif-export import-data "Tweety-export.xml")

(define export-data (lkif-import "Tweety-export.xml"))



(define import-stages (lkif-data-stages import-data))

(define import-st1 (car import-stages))

;(view (stage-argument-graph import-st1) (stage-context import-st1))


(define export-stages (lkif-data-stages export-data))

(define export-st1 (car export-stages))

;(view (stage-argument-graph export-st1) (stage-context export-st1))