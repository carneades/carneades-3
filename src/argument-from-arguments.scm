(module argument-from-arguments mzscheme
  
  ; generator for arguments from argument graphs
  
  ; Essentially this generator interprets arguments as propositional implications ("rules") 
  ; and uses defeasible modus ponens to generate arguments from these implications.
  
  (require "stream.scm")
  (require "argument.scm")
  (require "statement.scm")
  (require "argument-search.scm")
  
  (provide generate-arguments-from-argument-graph)
  
  ; type generator: statement state  -> (stream-of response)
 
  ; generate-arguments-from-argument-graph: argument-graph -> generator
  (define (generate-arguments-from-argument-graph ag1)
    (lambda (goal state)
      (let ((ag2 (state-arguments state))
            (subs (state-substitutions state)))
        (let* ((p (subs goal))
               (node (get-node ag1 (statement-atom p))))
          (stream-flatmap 
           (lambda (arg-id)
             (let ((arg (get-argument ag1 arg-id)))
               (if arg
                   (let ((arg  (make-argument (gensym 'a) ; new id required to assure uniqueness
                                              (argument-direction arg)
                                              (argument-conclusion arg)
                                              (argument-premises arg)
                                              (argument-scheme arg))))
                     (stream (make-response subs arg))) ; no new substitutions, since propositional
                   ; else fail, no argument with the given id
                   (stream))))
           (apply stream (if (not node)
                             null
                             (if (statement-negative? p)
                                 (node-con node)
                                 (node-pro node)))))))))
  
  ) ; end of module