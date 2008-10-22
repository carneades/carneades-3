#!r6rs

(library 
 (carneades lib pstk system)
 
 (export run-program keyword? keyword->string)
 
 (import (rnrs)
         (core primitives))
 
 (define run-program
   (lambda (program)
     (let ((l (process program "text" "text" "")))
       (let ((utf8-transcoder (make-transcoder (utf-8-codec))))
         (list (transcoded-port (list-ref l 1) utf8-transcoder)
               (transcoded-port (list-ref l 2) utf8-transcoder))))))
 
 (define (keyword? x) #f)
 (define (keyword->string x) x)
 
 )

