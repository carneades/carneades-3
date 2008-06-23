#!r6rs
(library (carneades srfi parameters)
  (export 
    make-parameter 
    parameterize)
  (import 
    (only (scheme base) make-parameter parameterize))
)
