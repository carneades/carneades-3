#!r6rs
(library (carneades srfi general-cond)
  (export
    (rename (general-cond cond)))
  (import
    (rnrs))
  
  (define-syntax general-cond
    (lambda (stx)
      (syntax-case stx (=>)
        [(_ clauses ...)
         (with-syntax ([(ours ...)
                        (map (lambda (c)
                               (syntax-case c (=>)
                                 [(generator guard => receiver)
                                  #'((let-values ([vals generator])
                                       (and (apply guard vals)
                                            vals))
                                     => (lambda (vals)
                                          (apply receiver vals)))]
                                 [_ c]))
                             #'(clauses ...))])
           #'(cond ours ...))])))
)
