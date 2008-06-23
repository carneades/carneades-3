#!r6rs
(library (carneades srfi strings)
  (export
    string-map string-map!
    string-fold       string-unfold
    string-fold-right string-unfold-right 
    string-tabulate string-for-each string-for-each-index
    string-every string-any
    string-hash string-hash-ci
    string-compare string-compare-ci
    string=    string<    string>    string<=    string>=    string<>
    string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<> 
    string-downcase  string-upcase  string-titlecase  
    string-downcase! string-upcase! string-titlecase! 
    string-take string-take-right
    string-drop string-drop-right
    string-pad string-pad-right
    string-trim string-trim-right string-trim-both
    string-filter string-delete
    string-index string-index-right 
    string-skip  string-skip-right
    string-count
    string-prefix-length string-prefix-length-ci
    string-suffix-length string-suffix-length-ci
    string-prefix? string-prefix-ci?
    string-suffix? string-suffix-ci?
    string-contains string-contains-ci
    string-copy! substring/shared
    string-reverse string-reverse! reverse-list->string
    string-concatenate string-concatenate/shared string-concatenate-reverse
    string-append/shared
    xsubstring string-xcopy!
    string-null?
    string-join
    string-tokenize
    string-replace
    ; R5RS extended:
    string->list string-copy string-fill! 
    ; R5RS re-exports:
    string? make-string string-length string-ref string-set! 
    string string-append list->string
    ; Low-level routines:
    #;(make-kmp-restart-vector string-kmp-partial-search kmp-step
    string-parse-start+end
    string-parse-final-start+end
    let-string-start+end
    check-substring-spec
    substring-spec-ok?)
    )
  (import
    (except (rnrs base) error string-copy string-for-each string->list)
    (except (rnrs mutable-strings) string-fill!)
    (except (rnrs unicode) string-upcase string-downcase string-titlecase)
    (rnrs arithmetic bitwise)
    (rnrs control)
    (rnrs r5rs)
    (rnrs syntax-case)
    (prefix (carneades srfi error-reporting) ER:)
    (carneades srfi receive)
    (carneades srfi char-set)
    (carneades srfi parameters)
    (carneades srfi private let-opt)
    (carneades srfi private include-resolve))
  
  (define (error . args)
    (parameterize ([ER:error-who 
                    '(library (carneades srfi strings/13))])
      (apply ER:error args)))
  
  (define-syntax check-arg
    (lambda (stx)
      (syntax-case stx ()
        [(_ pred val caller)
         (and (identifier? #'val) (identifier? #'caller))
         #'(unless (pred val)
             (parameterize ([ER:error-who 'caller])
               (ER:error "check-arg failed" val)))])))
  
  (define (char-cased? c)
    (char-upper-case? (char-upcase c)))
  
  (include/resolve ("carneades" "srfi" "strings") "srfi-13.scm")
)
