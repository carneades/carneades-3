#!r6rs
(library (carneades srfi char-set)
  (export
    ; Predicates & comparison
    char-set? char-set= char-set<= char-set-hash
    ; Iterating over character sets
    char-set-cursor char-set-ref char-set-cursor-next end-of-char-set? 
    char-set-fold char-set-unfold char-set-unfold!
    char-set-for-each char-set-map
    ; Creating character sets
    char-set-copy char-set
    list->char-set  string->char-set
    list->char-set! string->char-set!
    char-set-filter  ucs-range->char-set 
    char-set-filter! ucs-range->char-set!
    ->char-set
    ; Querying character sets
    char-set->list char-set->string
    char-set-size char-set-count char-set-contains?
    char-set-every char-set-any
    ; Character-set algebra
    char-set-adjoin  char-set-delete
    char-set-adjoin! char-set-delete!
    char-set-complement  char-set-union  char-set-intersection
    char-set-complement! char-set-union! char-set-intersection!
    char-set-difference  char-set-xor  char-set-diff+intersection
    char-set-difference! char-set-xor! char-set-diff+intersection!
    ; Standard character sets
    char-set:lower-case  char-set:upper-case  char-set:title-case
    char-set:letter      char-set:digit       char-set:letter+digit
    char-set:graphic     char-set:printing    char-set:whitespace
    char-set:iso-control char-set:punctuation char-set:symbol
    char-set:hex-digit   char-set:blank       char-set:ascii
    char-set:empty       char-set:full
    )
  (import
    (except (rnrs base) error)
    (rnrs mutable-strings)
    (rnrs r5rs)
    (rnrs arithmetic bitwise)
    (rnrs control)
    (rnrs syntax-case)
    (prefix (carneades srfi error-reporting) ER:)
    (carneades srfi records)
    (carneades srfi parameters)
    (carneades srfi private let-opt)
    (carneades srfi private include-resolve))
  
  (define (%latin1->char i)
    (integer->char i))
  
  (define (%char->latin1 c)
    (char->integer c))
  
  (define (error . args)
    (parameterize ([ER:error-who 
                    '(library (carneades srfi char-set/14))])
      (apply ER:error args)))
    
  (define-syntax check-arg
    (lambda (stx)
      (syntax-case stx ()
        [(_ pred val caller)
         (identifier? #'val)
         #'(unless (pred val)
             (parameterize ([ER:error-who caller])
               (ER:error "check-arg failed" val)))])))
  
  (include/resolve ("carneades" "srfi" "char-set") "srfi-14.scm")
)
