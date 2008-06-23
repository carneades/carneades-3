#!r6rs
(library (carneades srfi string-ports)
  (export
    open-input-string
    open-output-string
    get-output-string)
  (import
    (rnrs base)
    (only (rnrs io ports) open-string-input-port)
    (carneades srfi string-ports compat))
  
  (define (open-input-string str)
    (open-string-input-port str))
)
