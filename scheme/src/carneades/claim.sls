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
 (carneades claim)
 
 ; Generate arguments from claims made by a user and provides a way to ask the user questions. 
 ; Designed for use with form servers.
 
 (export make-claim generate-arguments-from-claims)

 (import (rnrs)
         (carneades base)
         (carneades stream)
         (carneades unify)
         (carneades statement)
         (prefix (carneades argument) argument:)
         (carneades argument-search)
         (carneades rule)
         (carneades lib srfi format) 
         (prefix (carneades lib srfi lists) list:)
         (carneades lib match)
         (only (carneades system) gensym))
 
 
 (define-record-type claim
   (fields id          ; symbol
           statement   ; statement
           ))
 
 (define (claim->rule claim)
   (make-rule (claim-id claim)
              #t
              (list (claim-statement claim)) ; head
              null))                         ; body
   
 ; claims->rulebase: (listof claim) -> rulebase
 (define (claims->rulebase claims)
   (add-rules empty-rulebase (map claim->rule claims)))
 
 (define (askable? goal questions)
   (cond ((constant? goal) 
          (member goal questions))
         ((compound-term? goal)
          (and
           (member (statement-predicate goal) questions)
           (or (null? (term-args goal))
               (constant? (car (term-args goal))))))
         (else #f)))
   
 ; type generator: statement state -> (stream-of response)
 
 ; generate-arguments-from-claims: claims (listof statement) -> generator
 (define (generate-arguments-from-claims claims questions)
   (lambda (goal state) 
      (let ((results ((generate-arguments-from-rules (claims->rulebase claims) null)
                      goal
                      state)))
        (if (stream-null? results)
            (let ((s ((state-substitutions state) goal)))
                 (if (askable? s questions)
                     (raise `(ask ,(statement-wff s)))
                     (stream))) ; empty stream
            results))))
            
 
 ) ;end of module
