#!r6rs

(library
 
 (carneades lib xml sxml force)
 
 (export force delay)
 
 (import (rnrs))

 (define force
   (lambda (object)
     (object)))
 
 (define-syntax delay
   (syntax-rules ()
     ((delay expression)
      (make-promise (lambda () expression)))))
 

 (define make-promise
   (lambda (proc)
     (let ((result-ready? #f)
           (result #f))
       (lambda ()
         (if result-ready?
             result
             (let ((x (proc)))
               (if result-ready?
                   result
                   (begin (set! result-ready? #t)
                          (set! result x)
                          result))))))))
 
 )