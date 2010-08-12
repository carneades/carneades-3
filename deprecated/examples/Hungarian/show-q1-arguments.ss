#!r6rs

(import (rnrs)
        (carneades lkif2)
        (carneades argument-diagram))


(define import-data (lkif-import "q1-arguments.xml"))

(define graphs (lkif-data-argument-graphs import-data))

(define ag1 (car graphs))

(view ag1)