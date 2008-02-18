(module argument-builtins mzscheme
  (require "stream.scm") 
  (require "unify.scm")
  (require (lib "match.ss"))
  (require "rule.scm")
  (require "argument-search.scm")
  (require (prefix argument: "argument.scm"))
  
  (provide builtins)
  
  ; An argument generator for "builtin" predicates: eval, etc.
  
  ; type generator: statement argument-graph substitutions  -> 
  ;                 (stream-of (pair-of argument substitutions))
  
  (define builtin-rules 
    (rulebase 
     (rule* priority1   
            (if (and (applies ?r2 ?p1)
                     (prior ?r2 ?r1))
                (priority ?r2 ?r1 (not ?p1))))
     
     (rule* priority2
            (if (and (applies ?r2 (not ?p1)) 
                     (prior ?r2 ?r1))
                (priority ?r2 ?r1 ?p1)))
     ))
  
  ; dispatch: stmt state -> (stream-of response)
  (define (dispatch stmt state)
    (let ((args (state-arguments state))
          (subs (state-substitutions state)))
      (match stmt
        (('eval term expr) 
         (with-handlers 
             ; fail if any exception is raised
             (((lambda (x) #t) (lambda (exn) (stream))))
           (let* ((result (eval (subs expr))) ; to do: eval in a safer namespace
                  (subs2 (unify* term
                                 result 
                                 subs 
                                 (lambda (t) t) 
                                 (lambda (msg) #f) 
                                 #f)))
             (if (not subs2)
                 (stream) ; not unifiable, so fail by returning the empty stream
                 (stream 
                  (make-response 
                   subs2
                   (argument:make-argument 
                    (gensym 'a) ; id
                    ; direction:
                    'pro
                    ; conclusion:
                    stmt
                    ; premises:
                    null
                    ; scheme:
                    "builtin: eval")))))))
        (_ (stream))))) ; fail
  
  ; builtins: statement argument-graph substitutions -> 
  ;           (stream-of (pair-of argument substitutions)
  (define (builtins goal state)
      (stream-interleave
       ((generate-arguments-from-rules builtin-rules null) goal state)
       (dispatch goal state)))
  
  
  ) ; end of builtin.scm