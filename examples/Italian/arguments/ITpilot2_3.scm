(module ITpilot2_3 mzscheme
    
  (require (prefix lkif: "../../../src/lkif.scm"))
  (require "../../../src/statement.scm")
  (require "../../../src/argument.scm")
  (require "../../../src/argument-diagram.scm")
  (require (prefix list: (lib "list.ss" "srfi" "1")))
  
  (define imports (lkif:import "ITpilot2_3.xml"))
  (define texts (index-by-statement (list:filter text? imports)))
  
  (define ag1 (car (list:filter argument-graph? imports)))
  
  (define c1 (accept default-context ()))
 ; (define c2 (reject c1 'email))
  
  (define (show ag context)
    (view* ag 
           context 
           (lambda (x) x)
           (lambda (s) 
             (let ((txt (hash-table-get texts s (lambda (x) #f))))
               (if (and txt (not (equal? (text-summary txt) "")))
                   (text-summary txt)
                   s)))))
  
  (show ag1 c1)
 ; (show ag1 c2)
  ) ; module end