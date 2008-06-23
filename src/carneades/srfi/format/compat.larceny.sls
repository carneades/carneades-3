(library (carneades srfi format compat)
  (export
    pretty-print
    ascii-tab)
  (import
    (r5rs) ;; define
    (primitives pretty-print))
  
  (define ascii-tab #\tab)
)
