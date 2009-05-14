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
 
 
 ; reply: state statement -> state
 ; reply constructs a successor state from an answer by 
 ; unifying the answer with the current goal of the prior state 
 ; and asserting an argument claiming the answer is true.
 ; If the answer is unifiable with the current goal, then the goal is 
 ; considered solved by the answer and removed from the
 ; list of goals in the successor state.
 (define (reply state answer)
   (let* ((question (current-goal state))
          (subs1 (state-substitutions state))
          (subs2 (and question 
                      (unify* question
                              answer
                              subs1                                        
                              (lambda (t) t) 
                              (lambda (msg) #f) 
                              #f)))
          (arg (make-argument (gensym 'a)
                              (if (statement-positive? question)
                                  'pro
                                  'con)
                              (statement-atom answer) 
                              null 
                              "claim")))
     (if subs2
         (make-successor-state state (make-response subs2 arg))
         (replace-argument-graph state (assert-argument (state-arguments state) arg)))))
 
 ) ;end of module
