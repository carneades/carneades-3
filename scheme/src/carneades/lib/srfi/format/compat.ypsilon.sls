#!r6rs

(library (carneades lib srfi format compat)
  (export pretty-print ascii-tab)
  (import
    (rnrs base)
    (only (core) pretty-print))
  
  (define ascii-tab #\tab)  
)