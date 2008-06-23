#!r6rs
(library (carneades srfi args-fold)
  (export
    args-fold
    (rename (make-option option))
    option?
    option-names
    option-required-arg?
    option-optional-arg?
    option-processor)
  (import 
    (rnrs)
    (carneades srfi private include-resolve))
  
  
  (define-record-type option
    (fields 
      names required-arg? optional-arg? processor)
    (protocol 
      (lambda (c) 
        (lambda (n ra oa p)
          (if (and 
                (and (list? n)
                     (positive? (length n))
                     (for-all (lambda (x) 
                                (or (and (string? x) (positive? (string-length x))) 
                                    (char? x))) 
                              n))
                (boolean? ra)
                (boolean? oa)
                (not (and ra oa))
                (procedure? p))
            (c n ra oa p)
            (assertion-violation 'option "invalid arguments" n ra oa p))))))

  (define args-fold
    (let ([option make-option])
      (include/resolve ("carneades" "srfi" "args-fold") "srfi-37-reference.scm")
      args-fold))
)
