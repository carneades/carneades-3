#!r6rs
(library (carneades srfi rec)
  (export rec)
  (import (rnrs))
  
  ;;; Taken directly from the SRFI-31
  (define-syntax rec
    (syntax-rules ()
      [(rec (NAME . VARIABLES) . BODY)
       (letrec ( (NAME (lambda VARIABLES . BODY)) ) NAME)]
      [(rec NAME EXPRESSION)
       (letrec ( (NAME EXPRESSION) ) NAME)]))  
  
)
