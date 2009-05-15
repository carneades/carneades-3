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
 
 (export ask-user reply make-answer answer-predicate answer-subject answer-values answer-closed answer-statements)

 (import (rnrs)
         (carneades base)
         (only (carneades system) gensym)
         (carneades statement)
         (carneades stream)
         (carneades unify)
         (carneades argument)
         (prefix (carneades table) table:)
         (carneades argument-search))
   
 ; type generator: statement state -> (stream-of response)
 
 ; ask: (statement -> boolean) -> generator
 (define (ask-user askable?)
   (lambda (goal state) 
     (let ((g ((state-substitutions state) goal)))
       (if (askable? g)
           (raise-continuable `(ask ,g ,state))
           (stream))))) ; empty stream
 

 ; If the answer is closed, the absence of a value in the list of values can be used to 
 ; generate a con argument, using the "closed-world assumption".  A better term for this here
 ; might be the "closed-world assertion". Notice that this assertion is not global, but made
 ; for each property of each object, separately.  For example, if the 
 ; issue is (p s o), p is closed for s and o is not in the list of values for p of s, then
 ; an the testimony can be used to make an argument con (p s o).
 ; If the user does not know the answer to a question, and thus cannot provide some (or any) values,
 ; an answer record for the question will be nonetheless associated with the question in 
 ; the answers table of the user's testimony.  This answer will be open (not closed) and the 
 ; list of values provided may be empty. The presence of the answer in the answer table records
 ; that the question has been asked, however many values were supplied.
 (define-record-type answer
   (fields predicate   ; symbol
           subject     ; symbol
           values      ; (list-of datum)
           closed      ; boolean, true if all values have been provided
           )
   (sealed #t))
 
 ; answer-statements: answer -> (stream-of statement)
 (define (answer-statements answer)
   (list->stream (map (lambda (value)
                        (list (answer-predicate answer)
                              (answer-subject answer)
                              value))
                      (answer-values answer))))
  
 ; reply: state statement (list-of answer) -> (stream-of response)
 ; to do: handle negative goals, using the limited "closed-world assertion" discussed above.
 (define (reply state goal answers)
   (let* ((subs1 (state-substitutions state)))
     (stream-flatmap (lambda (stmt) 
                       (let ((subs2 (unify* goal
                                            stmt 
                                            subs1 
                                            (lambda (t) t) 
                                            (lambda (msg) #f)
                                            #f)))
                         (if (not subs2)
                             (stream) ; fail
                             (stream (make-response subs2 (make-argument (gensym 'a) 'pro stmt null "claim"))))))
                     (stream-flatmap answer-statements (list->stream answers)))))
 
 ) ;end of module
