#!r6rs
(library (carneades srfi cond-expand)
  (export 
    cond-expand)
  (import
    (rnrs)
    (for (carneades srfi private registry) expand))
  
  (define-syntax cond-expand
    (lambda (stx)
      (syntax-case stx (and or not else)
        [(_) 
         (syntax-violation #f "Unfulfilled cond-expand" stx)]
        [(_ (else body ...))
         #'(begin body ...)]
        [(_ ((and) body ...) more-clauses ...)
         #'(begin body ...)]
        [(_ ((and req1 req2 ...) body ...) more-clauses ...)
         #'(cond-expand
            (req1
             (cond-expand
              ((and req2 ...) body ...)
              more-clauses ...))
            more-clauses ...)]
        [(_ ((or) body ...) more-clauses ...)
         #'(cond-expand more-clauses ...)]
        [(_ ((or req1 req2 ...) body ...) more-clauses ...)
         #'(cond-expand
            (req1
             (begin body ...))
            (else
             (cond-expand
              ((or req2 ...) body ...)
              more-clauses ...)))]
        [(_ ((not req) body ...) more-clauses ...)
         #'(cond-expand
            (req
             (cond-expand more-clauses ...))
            (else body ...))]
        [(_ (feature-id body ...) more-clauses ...)
         (if (member (syntax->datum #'feature-id) available-features)
           #'(begin body ...)
           #'(cond-expand more-clauses ...))])))
  
)
