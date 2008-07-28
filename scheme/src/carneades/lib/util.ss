#!r6rs

(library
 
 (carneades lib util)
 
 (export sign
         quotient
         remainder
         modulo)
 
 (import (rnrs base))
 
 (define (sign n)
   (cond
     ((negative? n) -1)
     ((positive? n) 1)
     (else 0)))
 
 (define (quotient n1 n2)
   (* (sign n1) (sign n2) (div (abs n1) (abs n2))))
 
 (define (remainder n1 n2)
   (* (sign n1) (mod (abs n1) (abs n2))))
 
 (define (modulo n1 n2)
   (* (sign n2) (mod (* (sign n2) n1) (abs n2))))
 
 )