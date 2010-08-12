#!r6rs

(library 
 (carneades lib pstk system)
 
 (export keyword? keyword->string)
 (import (rnrs base)
         (ikarus))
 
 (define (keyword? x) #f)
 (define (keyword->string x) x)
 
 )
