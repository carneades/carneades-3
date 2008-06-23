#!r6rs
(library (carneades srfi sharing)
  (export
    write-with-shared-structure
    (rename (write-with-shared-structure write/ss))
    read-with-shared-structure
    (rename (read-with-shared-structure read/ss)))
  (import
    (rnrs)
    (only (scheme base) parameterize print-graph read-accept-graph))
  
  (define (write-with-shared-structure . args)
    (parameterize ([print-graph #t]) 
      (apply write args)))
  
  (define (read-with-shared-structure . args)
    (parameterize ([read-accept-graph #t]) 
      (apply read args)))
  
)
