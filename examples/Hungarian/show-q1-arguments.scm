(module show-q1-arguments mzscheme
    
  (require (prefix lkif: (lib "lkif.ss" "carneades")))
  (require (lib "statement.ss" "carneades"))
  (require (lib "argument.ss" "carneades"))
  (require (lib "argument-diagram.ss" "carneades"))
  (require (prefix list: (lib "list.ss" "srfi" "1")))
  
  (define imports (lkif:import "q1-arguments.xml"))
  (define texts (index-by-statement (list:filter text? imports)))
  
  (define ag1 (car (list:filter argument-graph? imports)))
  
  (define c1 (accept default-context (list 'deed)))
  (define c2 (reject c1 (list 'email)))
  
  (define (show ag context)
    (view* ag 
           context 
           (lambda (x) x)
           (lambda (s) 
             (let ((txt (hash-table-get texts s (lambda () #f))))
               (if (and txt (not (equal? (text-summary txt) "")))
                   (text-summary txt)
                   s)))))
  
  ; (show ag1 c1)
   (show ag1 c2)
  ) ; module end