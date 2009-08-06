#!r6rs

(library (carneades lib srfi sharing)
  (export  write-with-shared-structure
           (rename (write-with-shared-structure write/ss))
           read-with-shared-structure
           (rename (read-with-shared-structure read/ss)))
  (import
    (rnrs)
    (only (ikarus) print-graph parameterize))
  
  (define write-with-shared-structure
    (case-lambda
      [(obj port optarg)
       (assertion-violation 'write-with-shared-structure
         "this implementation does not support optarg")] 
      [args
       #;(unless (output-port? port)  ;; ikarus's write type-checks port
         (assertion-violation 'write-with-shared-structure "not an output-port" port))
       (parameterize ([print-graph #t])
         (apply write args))]))
  
  (define read-with-shared-structure read)
  
)
