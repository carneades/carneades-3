#!r6rs

(import (rnrs)
        (carneades lkif2)
        (carneades argument-diagram))

(define import-data (lkif-import "contract.xml"))

(define argument-graphs (lkif-data-argument-graphs import-data))

(define ag1 (car argument-graphs))

(view ag1)