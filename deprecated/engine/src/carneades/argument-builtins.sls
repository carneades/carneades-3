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

#!r6rs

(library
 (carneades argument-builtins)
 ; An argument generator for "builtin" predicates: eval, etc.

 
 (export builtins)
 
 (import (rnrs)
         ; (rnrs base)
         ; (rnrs exceptions)
         (rnrs eval)
         (carneades base)
         (carneades statement)
         (carneades stream)
         (carneades unify)
         (carneades lib match)
         (carneades rule)
         (carneades argument-search)
         (only (carneades system) gensym)
         (prefix (carneades argument) arg:))
 
 (define *debug* #f)
 
 ; type generator: statement state  -> (stream-of response)
 
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
   (let* ((args (state-arguments state))
          (subs (state-substitutions state))
          (wff (statement-wff stmt)))
     (if *debug* (begin (display "builtins: matching statement")
                        (newline)))
     (match wff
       ;eval
       (('eval term expr) 
        (call/cc (lambda (escape)
                   (with-exception-handler
                    ; fail if any exception is raised
                    (lambda (exn) (escape (stream)))
                    (lambda ()
                      (let* ((result (eval (apply-substitution subs (statement-wff expr))
                                           (environment '(rnrs)))) ; to do: use safer environment
                             (subs2 (unify* term
                                            result 
                                            subs 
                                            (lambda (t) t) 
                                            (lambda (msg) #f) 
                                            #f)))
                        (if *debug* (begin (display "builtins: matched eval")
                                           (newline)))
                        (if (not subs2)
                            (stream) ; not unifiable, so fail by returning the empty stream
                            (stream 
                             (make-response subs2
                                            (arg:make-argument 
                                             ; id:
                                             (gensym 'a)
                                             ; direction
                                             'pro
                                             ; conclusion:
                                             stmt
                                             ; premises:
                                             null
                                             ; scheme:
                                             "builtin:eval"))))))))))
       ; equal
       (('= term1 term2)
        (let ((subs2 (unify* term1
                             term2
                             subs 
                             (lambda (t) t) 
                             (lambda (msg) #f)
                             #f)))
          (if *debug* (begin (display "builtins: matched =")
                             (newline)))
          (if *debug* (printf "(= ~a ~a) => ~a~%" (apply-substitution subs term1) (apply-substitution subs term2) (if subs2 #t #f)))
          (if (not subs2)
              (stream) ; fail
              (stream (make-response subs2 
                                     (arg:make-argument
                                      ; id
                                      (gensym 'a)
                                      ; direction
                                      'pro
                                      ; conclusion
                                      stmt
                                      ; premises
                                      null
                                      ; scheme:
                                      "builtin:equal"))))))
       ;unequal
       (('!= term1 term2)
        (let ((subs2 (unify* term1
                             term2
                             subs 
                             (lambda (t) t) 
                             (lambda (msg) #f)
                             #f)))
          (if *debug* (begin (display "builtins: matched !=")
                             (newline)))
          (if *debug* (printf "(!= ~a ~a) => ~a~%" term1 term2 (if subs2 #f #t)))
          (if (not subs2)              
              (stream (make-response subs 
                                     (arg:make-argument
                                      ; id
                                      (gensym 'a)
                                      ; direction
                                      'pro
                                      ; conclusion
                                      stmt
                                      ; premises
                                      null
                                      ; scheme:
                                      "builtin:unequal")))
              (stream) ; fail
              )))
       
       (stmt 
        (begin (if *debug* (begin (display "builtins: matched nothing: ")
                                  (write stmt)
                                  (newline)))
               ; try to unify stmt with statements in the argument graph
               ; no new arguments are added, but the substitutions are extended
               (stream-flatmap (lambda (stmt2) 
                                 (let ((subs2 (unify* stmt
                                                      stmt2
                                                      subs 
                                                      (lambda (t) t) 
                                                      (lambda (msg) #f)
                                                      #f)))
                                   (if *debug* (printf "(unify ~a ~a) => ~a~%" (statement-atom stmt) stmt2 (if subs2 #t #f)))
                                   (if (not subs2)
                                       (stream) ; fail
                                       (stream (make-response subs2 #f)))))
                               (list->stream (arg:in-statements args (statement-predicate stmt)))))))))
 
 ; builtins: statement state -> (stream-of response)
 (define (builtins goal state)
   (stream-interleave
    ((generate-arguments-from-rules builtin-rules null) goal state)
    (dispatch goal state)))
 
 
 ) ; end of argument builtins library