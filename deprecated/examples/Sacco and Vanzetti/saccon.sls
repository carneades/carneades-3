; the imported lkif-file was constructed by Floris Bex florisbex@gmail.com

#!r6rs

(import (rnrs)
        (carneades argument)
        (carneades argument-diagram)
        (carneades lkif2))

(define data (lkif-import "SaccoCrimeSceneCarneades.xml"))

(define ag (car (lkif-data-argument-graphs data)))

(view ag)
