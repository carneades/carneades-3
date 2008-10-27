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
 
 (export make-engine make-engine* show-state show show1 ask ask1 diagram1
         all-in? some-in? some-argument-found? no-argument-found?)
 
 (import (except (rnrs base) assert)
         (rnrs io simple)
         (carneades argument)
         (carneades argument-search)
         (carneades stream)
         (carneades argument-diagram)
         (carneades statement)
         (prefix (carneades search) search:)
         (carneades lib srfi format))
 
 (define (printf format-string . args)
    (apply format `(,(current-output-port)  ,format-string ,@args)))
 
 ; make-engine*: integer integer context (list-of generator) -> statement -> (stream-of argument-state)
 (define (make-engine* max-nodes max-turns context generators) 
   (lambda (goal)
     (find-best-arguments search:depth-first 
                          (search:make-resource max-nodes)
                          max-turns
                          (initial-state goal context) 
                          generators)))
 
 ; make-engine: integer integer (list-of generator) -> statement -> (stream-of argument-state)
 ; a simplified version of make-engine*, using the default-context 
 (define (make-engine max-nodes max-turns generators) 
   (make-engine* max-nodes max-turns default-context generators))
 
 ; show-state: state -> void
 ; view a diagram of the argument graph of a state
 (define (show-state s)
   (view* (state-arguments s)
          (state-context s)
          (context-substitutions (state-context s))
          (lambda (s) (format "~A" s))))
 
 
 ; show: statement (statement -> (stream-of argument-state)) -> void
 ; for each state in the stream, view a diagram of argument graph of the state
 (define (show query engine)
   (let ((str (engine query)))
     (stream-for-each (lambda (s) (show-state s)) str)))
 
 ; show1: statement (statement -> (stream-of argument-state)) -> void
 ; view a diagram of argument graph of the first state in a stream of states.
 (define (show1 query engine)
   (let ((str (engine query)))
     (if (not (stream-null? str)) 
         (show-state (stream-car str)))))

 ; diagram1: statement (statement -> (stream-of argument-state)) -> void
 ; writes a DOT diagram for argument graph of the first state in the stream
 (define (diagram1 query engine)
   (let ((str (engine query)))
     (if (not (stream-null? str)) 
         (diagram* (state-arguments str)
                   (state-context str)
                   (context-substitutions (state-context str))
                   (lambda (s) (format "~A" s))
                   (current-output-port)))))
 
 ; ask: statement (statement -> (stream-of argument-state)) -> void
 ; displays each solution to a query, using the given inference engine
 ; with any variables instantiated using the substituions of the solution state found, 
 ; or prints nothing if no solution was found. Always terminates, as only
 ; solutions found given the resource limit of the inference engine will be displayed.
 (define (ask query engine)
   (let ((str (engine query)))
     (stream-for-each (lambda (s) 
                        (if (in? (state-arguments s)
                                         (state-context s)
                                         query)
                            (printf "~A~%" ((context-substitutions (state-context s)) query))))
                      str)))
 
 ; ask1: statement (statement -> (stream-of argument-state)) -> void
 ; displays the first solution to a query, using the given inference engine,
 ; with any variables instantiated using the substituions of the solution state found, 
 ; or prints nothing if no solution was found.
 (define (ask1 query engine)
   (define (f str) 
     (if (not (stream-null? str))
         (let ((s (stream-car str)))
           (if (in? (state-arguments s)
                    (state-context s)
                    query)
               (printf "~A~%" ((context-substitutions (state-context s)) query))
               (f (stream-cdr str))))))
   (let ((str (engine query)))
     (f str)))
 
 
 ; all-in? : statement engine -> boolean
 ; Checks if at least one argument graph was found by the engine and the
 ; statement is acceptable or accepted in every argument graph found.
 (define (all-in? query engine)
   (let ((str (engine query)))
     (and (not (stream-null? str))
          (stream-null? (stream-filter (lambda (s)
                                         (not (in? (state-arguments s)
                                                           (state-context s)
                                                           query)))
                                       str)))))
 
 ; some-in? : statement engine -> boolean
 ; Checks if at least one argument graph was found in which the statement is acceptable or accepted.  
 (define (some-in? query engine)
   (let ((str (engine query)))
     (not (stream-null? (stream-filter (lambda (s)
                                         (in? (state-arguments s)
                                                      (state-context s)
                                                      query))
                                       str)))))
 
 ; some-argument-found? : statement engine -> boolean
 ; Returns true if the given engine is able to find at least one argument pro or con the statement
 ; of the query.
 (define (some-argument-found? query engine) (not (stream-null? (engine query))))
 
 ; no-argument-found? : statement engine -> boolean
 ; Returns true if the given inference engine is not able to find any arguments pro or
 ; con the statement
 (define (no-argument-found? query engine) (stream-null? (engine query)))
 
 ) ; end of shell.scm