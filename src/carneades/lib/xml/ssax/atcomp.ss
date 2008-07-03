#!r6rs

(library
 
 (carneades lib xml ssax atcomp)
 
 (export at dat pp)
 
 (import (rnrs))
 
 (define-syntax at
   (syntax-rules ()
     ((_) (string->symbol "@"))))
 
 (define-syntax dat
    (syntax-rules ()
      ((_) (string->symbol "@@"))))
 
 (define-syntax pp
   (syntax-rules ()
     ((_) (string->symbol ".."))))
 
 )