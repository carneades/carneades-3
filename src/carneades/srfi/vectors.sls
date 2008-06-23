#!r6rs
(library (carneades srfi vectors)
  (export
    ;;; * Constructors
    make-vector vector
    vector-unfold         vector-unfold-right
    vector-copy           vector-reverse-copy
    vector-append         vector-concatenate
    ;;; * Predicates
    vector?
    vector-empty?
    vector=
    ;;; * Selectors
    vector-ref
    vector-length
    ;;; * Iteration
    vector-fold           vector-fold-right
    vector-map            vector-map!
    vector-for-each
    vector-count
    ;;; * Searching
    vector-index          vector-skip
    vector-index-right    vector-skip-right
    vector-binary-search  vector-any    vector-every
    ;;; * Mutators
    vector-set!
    vector-swap!
    vector-fill!
    vector-reverse!
    vector-copy!          vector-reverse-copy!
    ;;; * Conversion
    vector->list          reverse-vector->list
    list->vector          reverse-list->vector )
  (import
    (except (rnrs) error vector-map vector-for-each vector-fill! vector->list
                   list->vector)
    (prefix (only (rnrs) vector-fill! vector->list list->vector) rnrs:)
    (rnrs r5rs)
    (prefix (carneades srfi error-reporting) ER:)
    (carneades srfi parameters)
    (carneades srfi receive)
    (carneades srfi private include-resolve))
  
  (define (error . args)
    (parameterize ([ER:error-who 
                    '(library (carneades srfi vectors/43))])
      (apply ER:error args)))
  
  (define-syntax check-type
    (lambda (stx)
      (syntax-case stx ()
        [(_ pred? value callee)
         (if (identifier? #'value)
           #'(if (pred? value)
               value
               (parameterize ([ER:error-who callee])
                 (ER:error "erroneous value" value)))
           #'(let ([v value])
               (if (pred? v)
                 v
                 (parameterize ([ER:error-who callee])
                   (ER:error "erroneous value" v)))))])))
    
  (include/resolve ("carneades" "srfi" "vectors") "vector-lib.scm")
)
