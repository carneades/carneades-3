#!r6rs
(library (carneades srfi random)
  (export random-integer
          random-real
          default-random-source
          make-random-source
          random-source?
          random-source-state-ref
          random-source-state-set!
          random-source-randomize!
          random-source-pseudo-randomize!
          random-source-make-integers
          random-source-make-reals)
  
  (import (except (rnrs) error)
          (rnrs r5rs)
          (carneades srfi parameters)
          (only (carneades srfi time) time-nanosecond current-time)
          (prefix (carneades srfi error-reporting) ER:)
          (carneades srfi private include-resolve)
          )
  
  (define (error . args)
    (parameterize ([ER:error-who 
                    '(library (carneades srfi random/27))])
      (apply ER:error args)))
  
   (include/resolve ("carneades" "srfi" "random") "random.ss")
  )
