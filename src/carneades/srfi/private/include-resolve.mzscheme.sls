#!r6rs
(library (carneades srfi private include-resolve)
  (export 
    include/resolve)
  (import 
    (rnrs)
    (only (scheme base) lib)
    (only (scheme include) include))
  
  (define-syntax include/resolve
    (lambda (stx)
      (syntax-case stx ()
        [(kw (lib-path* ...) file-path)
         (for-all (lambda (s) (and (string? s) (positive? (string-length s)))) 
                  (syntax->datum #'(file-path lib-path* ...)))
         (with-syntax ([collects-path 
                        (apply string-append
                               (append
                                 (map (lambda (lps) (string-append lps "/"))
                                      (syntax->datum #'(lib-path* ...)))
                                 (list (syntax->datum #'file-path))))])
           #'(include (lib collects-path)))])))
)
