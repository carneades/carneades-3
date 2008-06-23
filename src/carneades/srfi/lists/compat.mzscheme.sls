#!r6rs
(library (carneades srfi lists compat)
  (export 
    last-pair make-list)
  (import 
    (rnrs)
    (only (scheme base) void sub1))  
  
  (define (last-pair x)
    (unless (pair? x)
      (assertion-violation 'last-pair "not a pair" x))
    (let loop ([x x])
      (if (pair? (cdr x))
        (loop (cdr x))
        x)))
  
  (define make-list
    (case-lambda 
      [(n) (make-list n (void))]
      [(n v)
       (unless (and (integer? n) (exact? n) (not (negative? n)))
         (assertion-violation 'make-list "not a valid length" n))
       (let loop ([n n] [r '()])
         (if (= 0 n)
           r
           (loop (sub1 n) (cons v r))))]))
  
)
