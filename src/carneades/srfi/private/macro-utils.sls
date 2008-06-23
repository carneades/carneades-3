#!r6rs
(library (carneades srfi private macro-utils)
  (export
    duplicate-id
    unique-ids?
    unique-ids?/raise
    formals-ok?)
  (import
    (rnrs))
  
  (define (duplicate-id ls)
    (if (null? ls)
      #f
      (or
        (let loop ([x (car ls)] [rest (cdr ls)])
          (if (null? rest)
            #f
            (if (bound-identifier=? x (car rest))
              x
              (loop x (cdr rest)))))
        (duplicate-id (cdr ls)))))
  
  (define (unique-ids? ls)
    (not (duplicate-id ls)))
  
  (define unique-ids?/raise
    (case-lambda
      [(ids stx msg)
       (let ([dup (duplicate-id ids)])
         (if dup
           (syntax-violation #f msg stx dup)
           #t))]
      [(ids stx)
       (unique-ids?/raise ids stx "duplicate identifier")]))
  
  (define (formals-ok? frmls-stx orig-stx)
    (syntax-case frmls-stx ()
      [(arg* ... . rest)
       (and (or (null? (syntax->datum #'rest))
                (identifier? #'rest)
                (syntax-violation #f "not an identifier" orig-stx #'rest))
            (for-all (lambda (id)
                       (or (identifier? id)
                           (syntax-violation #f "not an identifier" orig-stx id)))
                     #'(arg* ...))
            (unique-ids?/raise 
              (append
                #'(arg* ...)
                (if (identifier? #'rest) (list #'rest) '())) 
              orig-stx))]))
)
