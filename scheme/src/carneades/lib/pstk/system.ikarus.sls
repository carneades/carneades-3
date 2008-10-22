#!r6rs

(library 
 (carneades lib pstk system)
 
 (export run-program keyword? keyword->string)
 (import (rnrs base)
         (ikarus))
  
 (define run-program
   (lambda (program)
     (let-values (((pid from to errport) (process program)))
       (let ((utf8-transcoder (make-transcoder (utf-8-codec))))
         (list (transcoded-port to   utf8-transcoder)
               (transcoded-port from utf8-transcoder))))))
 
 (define (keyword? x) #f)
 (define (keyword->string x) x)
 
 )
