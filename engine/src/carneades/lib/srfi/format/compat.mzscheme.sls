#!r6rs
(library 
 (carneades srfi format compat)
 (export pretty-print ascii-tab)
 
 (import
  (rnrs base)
  (only (scheme pretty) pretty-print))
 
 (define ascii-tab #\tab)
 )
