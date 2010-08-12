#!r6rs
(library (carneades lib srfi string-ports)
  (export
    open-input-string
    open-output-string
    get-output-string)
  (import
    (rnrs base)
    (only (rnrs io ports) open-string-input-port)
    (carneades lib srfi string-ports compat))
  
  (define (open-input-string str)
    (open-string-input-port str))
)
