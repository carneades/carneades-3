#!r6rs

(library 
 (carneades lib pstk system)
 
 (export run-program keyword? keyword->string)
 
 (import (rnrs)
         (core primitives))
  
 (define run-program
    (lambda (program)
      (let ((l (process program)))
        (list (transcoded-port (list-ref l 2) (native-transcoder))
              (transcoded-port (list-ref l 1) (native-transcoder))))))
 
 (define (keyword? x) #f)
 (define (keyword->string x) x)
 
 )

