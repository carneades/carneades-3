(module kt-ex1 mzscheme
  
  (require (prefix lkif: (lib "lkif.scm" "carneades")))
  (require (lib "statement.scm" "carneades"))
  (require (lib "shell.scm" "carneades"))
  (require (lib "stream.scm" "carneades"))
  (require (lib "argument-search.scm" "carneades"))
  (require (prefix argument: (lib "argument.scm" "carneades")))
  (require (prefix e: (lib "evidence.scm" "carneades")))
  (require (lib "argument-diagram.scm" "carneades"))
  (require (lib "argument-from-arguments.scm" "carneades"))
  (require (planet "test.ss" ("schematics" "schemeunit.plt")))
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt")))
  
  
  (require (prefix list: (lib "list.ss" "srfi" "1")))
  
  (define imports (lkif:import "kt-ex1.xml"))
  (define texts (index-by-id (list:filter text? imports)))  
  (define ag1 (car (list:filter argument:argument-graph? imports)))
  
  (define witness (e:make-witness "Gerd"))
  
  (define form1 
    (e:make-form 
     ; questions
     (list (e:make-question 'k473 'boolean 'one "Max 10% of nominal value?"))
     ; help text, in SXML format
     null))
 
  (define testimony (e:make-testimony witness (list form1)))
  
 
  
   ; engine integer integer context -> statement -> (stream-of argument-state)
  (define (engine max-nodes max-turns context)
    (make-engine* max-nodes max-turns context
          (list  (e:generate-arguments-from-testimony testimony) ; ask the user first
                 (generate-arguments-from-argument-graph ag1))))
  
 
  (define c1 (argument:accept argument:default-context (list 'k473)))
  (define e1 (engine 20 1 argument:default-context))
 
  
  ; view1: statement (statement -> (stream-of argument-state)) -> void
  ; view a diagram of argument graph of the first state in a stream of states.
  (define (view1 query engine)
    (let ((str (engine query)))
      (if (not (stream-null? str)) 
          (let ((s (stream-car str)))
            (view* (state-arguments s)
                   (state-context s)
                   (lambda (x) x)
                   (lambda (stmt)
                     (let ((txt (hash-table-get texts stmt (lambda () #f))))
                       (if (and txt (not (equal? (text-summary txt) "")))
                           (text-summary txt)
                           stmt))))))))
  
  (define tests
    (test-suite
     "arguments from argument graphs"
     (test-true "q1" (all-acceptable? 'k473 e1))  ; max 10% of nominal value   
     (test-true "q2" (all-acceptable? 'k472 e1))  ; cash payment
     (test-true "q3" (all-acceptable? 'k470 e1))  ; counter-performance
     ))
  
   (test/text-ui tests)
  
  ; Answers to provide to questions asked:
  ; 1. Max 10% of nominal value?
  ; Answer: (all #t)
  
  
  ; (view1 'k470 e1) 
  
  ) ; module end