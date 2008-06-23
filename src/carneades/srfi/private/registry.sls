#!r6rs
(library (carneades srfi private registry)
  (export
    available-features)
  (import 
    (rnrs)
    (carneades srfi private implementation-features))
  
  (define aliases
    ; construct: ([primary [feature-aliases ...]] ...)
    (map 
      (lambda (x)
        (list (car x) `[,(string->symbol (string-append "srfi-" (number->string (cadr x))))]))
      ;      primary                SRFI code number
      '([(srfi cond-expand)             0]
        [(srfi lists)                   1]
        [(srfi and-let*)                2]
        [(srfi string-ports)            6]
        [(srfi receive)                 8]
        [(srfi records)                 9]
        [(srfi let-values)             11]
        [(srfi strings)                13]
        [(srfi char-set)               14]
        [(srfi case-lambda)            16]
        [(srfi time)                   19]
        [(srfi error-reporting)        23]
        [(srfi cut)                    26]
        [(srfi random)                 27]
        [(srfi rec)                    31]
        [(srfi args-fold)              37]
        [(srfi sharing)                38]
        [(srfi parameters)             39]
        [(srfi streams)                41]
        [(srfi eager-comprehensions)   42]
        [(srfi vectors)                43]
        [(srfi format)                 48]
        [(srfi general-cond)           61]
        [(srfi compare)                67]
        [(srfi lightweight-testing)    78])))
  
  (define available-features
    ; construct: (r6rs
    ;             implementation-features ...
    ;             primary0 feature-aliases0 ... 
    ;             ...)
    (apply append
           '(r6rs)
           implementation-features
           (map (lambda (al) (cons (car al) (cadr al)))
                aliases)))
  
  
)
