;;; Carneades Argumentation Library and Tools.
;;; Copyright (C) 2008 Thomas F. Gordon, Fraunhofer FOKUS, Berlin
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License version 3 (LGPL-3)
;;; as published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(module argument-builtins mzscheme
  (require "stream.ss") 
  (require "unify.ss")
  (require (lib "match.ss"))
  (require "rule.ss")
  (require "argument-search.ss")
  (require (prefix argument: "argument.ss"))
  
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