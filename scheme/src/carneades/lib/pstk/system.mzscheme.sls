#!r6rs


(library 
 (carneades lib pstk system)
 
 (export run-program keyword? keyword->string)
 
 (import (rnrs)
         (only (scheme base) subprocess keyword? keyword->string))
 
 
 (define (run-program program)
   (let-values
       (((pid in out err)
         (subprocess #f #f #f "/bin/sh" "-c"
                     (string-append "exec " program " 2>&1"))))
     (let ((utf8-transcoder (make-transcoder (utf-8-codec))))
       (list (transcoded-port in  utf8-transcoder)
             (transcoded-port out utf8-transcoder)))))
 
 )

