#!r6rs
(library (carneades srfi lightweight-testing compat)
  (export
    pretty-print/no-trailing-newline)
  (import
    (rnrs)
    (only (scheme pretty) pretty-print))
  
  ;;; check.scm says a pretty-print with a trailing newline 
  ;;; will make its print-outs look bad, so:
  (define pretty-print/no-trailing-newline
    (case-lambda
      [(datum output-port)
       (let* ([os (call-with-string-output-port (lambda (sop) (pretty-print datum sop)))]
              [os (if (and (positive? (string-length os))
                           (char=? #\newline (string-ref os (- (string-length os) 1))))
                    (substring os 0 (- (string-length os) 1))
                    os)])
         (display os output-port))]
      [(datum) 
       (pretty-print/no-trailing-newline datum (current-output-port))]))  
)
