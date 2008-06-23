#!r6rs
(library (carneades srfi and-let*)  
  (export 
    and-let*)
  (import 
    (rnrs)
    (for (only (carneades srfi private macro-utils) formals-ok?) expand))
  
  (define-syntax and-let*
    (lambda (stx)
      (define (get-id c)
        (syntax-case c () [(var expr) #'var] [_ #f]))
      (syntax-case stx ()
        [(_ (clause* ...) body* ...)
         (formals-ok? (filter values (map get-id #'(clause* ...))) stx)
         #'(and-let*-core #t (clause* ...) body* ...)])))
  
  (define-syntax and-let*-core
    (lambda (stx)
      (syntax-case stx ()
        [(kw _ ([var expr] clause* ...) body* ...)
         #'(let ([var expr])
             (if var
               (kw var (clause* ...) body* ...)
               #f))]
        [(kw _ ([expr] clause* ...) body* ...)
         #'(let ([t expr])
             (if t
               (kw t (clause* ...) body* ...)
               #f))]
        [(kw _ (id clause* ...) body* ...)
         (or (identifier? #'id)
             (syntax-violation #f "invalid clause" stx #'id))
         #'(if id
             (kw id (clause* ...) body* ...)
             #f)]
        [(kw last () body* ...)
         (if (positive? (length #'(body* ...)))
           #'(begin body* ...)
           #'last)])))
)
