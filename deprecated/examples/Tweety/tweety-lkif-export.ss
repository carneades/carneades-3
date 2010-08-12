#!r6rs

(import (rnrs)
        (carneades lkif2)
        (carneades argument-diagram))

(define import-data (lkif-import "Tweety.xml"))

(lkif-export '() import-data "Tweety-export.xml")

(define export-data (lkif-import "Tweety-export.xml"))

