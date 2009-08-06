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
 (carneades shell) ;; utility procedures for querying knowledge bases
 
 (export make-engine make-engine* show-state show show1 ask ask1 diagram1 succeed? fail?)
 
 (import (rnrs base)
         (rnrs io simple)
         (carneades base)
         (carneades argument)
         (carneades argument-search)
         (carneades stream)
         (carneades argument-diagram)
         (carneades statement)
         (prefix (carneades search) search:))

 
 ; make-engine*: integer integer argument-graph (list-of generator) -> statement -> (stream-of state)
 (define (make-engine* max-nodes max-turns ag generators) 
   (lambda (goal)
     (find-best-arguments search:depth-first 
                          (search:make-resource max-nodes)
                          max-turns
                          (initial-state goal ag) 
                          generators)))
 
 ; make-engine: integer integer (list-of generator) -> statement -> (stream-of state)
 ; a simplified version of make-engine*, using the default-context 
 (define (make-engine max-nodes max-turns generators) 
   (make-engine* max-nodes max-turns empty-argument-graph generators))
 
 ; show-state: state -> void
 ; view a diagram of the argument graph of a state
 (define (show-state s)
   (view* (state-arguments s)
          (lambda (s) (statement-formatted s))))
 
 ; solutions: (stream-of state) -> (stream-of state)
 ; A state is a "solution" if the instantiated topic of the state is in.
 (define (solutions str)
   (stream-filter (lambda (s) 
                    (in? (state-arguments s)
                    ((state-substitutions s) (state-topic s))))
;                    (and (eq? (state-viewpoint s) 'pro)
;                                  (goal-state? s)))
                  str))
 
 ; show: statement (statement -> (stream-of state)) -> void
 ; for each state in the stream, view a diagram of argument graph of the state
 (define (show query engine)
   (stream-for-each (lambda (s) (show-state s)) (engine query)))
 
 ; show1: statement (statement -> (stream-of state)) -> void
 ; view a diagram of argument graph of the first state
 (define (show1 query engine)
   (let ((str (engine query)))
     (if (not (stream-null? str)) 
         (show-state (stream-car str)))))

 ; diagram1: statement (statement -> (stream-of state)) -> void
 ; writes a DOT diagram for the argument graph of the first state in the stream
 (define (diagram1 query engine)
   (let ((str (engine query)))
     (if (not (stream-null? str)) 
         (diagram* (state-arguments (stream-car str))
                   (lambda (s) (statement-formatted s))
                   (current-output-port)))))
          
 ; ask: statement (statement -> (stream-of state)) -> void
 ; displays the query with the substitions found in each state 
 ; produced by the given inference engine or prints nothing if the stream is emtpy.
 ; Always terminates, as only states found given the resource limit of the 
 ; inference engine will be displayed.
 (define (ask query engine)
     (stream-for-each (lambda (s) (printf "~A~%" ((state-substitutions s) query)))
                      (solutions (engine query))))
 
 ; ask1: statement (statement -> (stream-of state)) -> void
 ; displays the query with the substitutions found in each state 
 ; produced by the given inference engine or prints nothing if the stream is empty.
 ; Always terminates, as only states found given the resource limit of the 
 ; inference engine will be displayed.
 (define (ask1 query engine)
   (let ((str (solutions (engine query))))
     (if (not (stream-null? str))
         (printf "~A~%" ((state-substitutions (stream-car str)) query)))))
 
 ; fail? : statement engine -> boolean
 ; True if no state found by the engine is a goal state
 (define (fail? query engine) (stream-null? (solutions (engine query))))
 
 ; succeed? : statement engine -> boolean
 ; True if at least one goal state was found by the engine
 (define (succeed? query engine)
   (not (stream-null? (solutions (engine query)))))


 
 ) ; end of shell.scm
