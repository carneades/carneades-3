#!r6rs
(library (carneades srfi lightweight-testing)
  (export
    check
    check-ec
    check-report
    check-set-mode!
    check-reset!
    check-passed?
    ;;; All of (carneades srfi eager-comprehensions):
    do-ec list-ec append-ec string-ec string-append-ec vector-ec 
    vector-of-length-ec sum-ec product-ec min-ec max-ec any?-ec 
    every?-ec first-ec last-ec fold-ec fold3-ec 
    : :list :string :vector :integers :range :real-range :char-range 
    :port :dispatched :do :let :parallel :while :until
    :-dispatch-ref :-dispatch-set! make-initial-:-dispatch 
    dispatch-union :generator-proc)
  (import 
    (except (rnrs) error)
    (carneades srfi lightweight-testing compat)
    (carneades srfi parameters)
    (carneades srfi private include-resolve)
    (prefix (carneades srfi error-reporting) ER:)
    (carneades srfi eager-comprehensions))
  
  (define (error . args)
    (parameterize ([ER:error-who
                    '(library (carneades srfi lightweight-testing/78))])
      (apply ER:error args)))
  
  (include/resolve ("carneades" "srfi" "lightweight-testing") "check.scm")
  
  (set! check:write pretty-print/no-trailing-newline)  
)
