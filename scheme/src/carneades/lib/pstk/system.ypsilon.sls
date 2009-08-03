#!r6rs

(library 
 (carneades lib pstk system)
 
 (export keyword? keyword->string)
 
 (import (rnrs)
         (core primitives))
 
 (define (keyword? x) #f)
 (define (keyword->string x) x)
 
 )

