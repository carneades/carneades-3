#!r6rs

(import (rnrs)
        (carneades lkif2)
        (rnrs io simple))

(define family-support (lkif-import "family-support.xml"))   

(lkif-export family-support (current-output-port))

; end of file