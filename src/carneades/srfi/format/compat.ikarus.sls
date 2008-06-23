(library (carneades srfi format compat)
  (export
    pretty-print
    ascii-tab)
  (import
    (rnrs base)
    (only (ikarus) pretty-print))
  
  (define ascii-tab #\tab)
  ;;(define pretty-print   write) ; ugly but permitted  
)
