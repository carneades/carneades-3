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
 (carneades ask)
 
 ; Provides a way to ask users questions. Designed for use with dialogue managers and form servers.
 
 (export ask-user reply)

 (import (rnrs)
         (carneades base)
         (only (carneades system) gensym)
         (carneades statement)
         (carneades stream)
         (carneades unify)
         (carneades argument)
         (carneades argument-search))
   
 ; type generator: statement state -> (stream-of response)
 
 ; ask: (statement -> boolean) -> generator
 (define (ask-user askable?)
   (lambda (goal state) 
     (let ((g ((state-substitutions state) goal)))
       (if (askable? g)
           (raise `(ask ,g ,state))
           (stream))))) ; empty stream
 
 
 ; reply: state statement statement -> response
 ; reply constructs a response from an answer by 
 ; unifying the answer and question using the substitutions
 ; of the prior state and constructing an argument from the answer.
 (define (reply state question answer)
   (let* ((subs1 (state-substitutions state))
          (subs2 (unify* question
                         answer
                         subs1                                        
                         (lambda (t) t) 
                         (lambda (msg) #f) 
                         #f)))
     (if (not subs2)
         state
         (make-response subs2
                        (make-argument (gensym 'a)
                                       (if (statement-positive? question)
                                           'pro
                                           'con)
                                       (statement-atom answer) 
                                       null 
                                       "ask")))))
 
 ) ;end of module
