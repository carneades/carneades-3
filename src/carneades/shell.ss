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
 
 (export make-engine make-engine show-state show show1 ask ask1
         all-acceptable? some-acceptable? success? failure?)
 
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
          (state-substitutions s)
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
 
 ; ask: statement (statement -> (stream-of argument-state)) -> void
 ; displays each solution to a query, using the given inference engine
 ; with any variables instantiated using the substituions of the solution state found, 
 ; or prints nothing if no solution was found. Always terminates, as only
 ; solutions found given the resource limit of the inference engine will be displayed.
 (define (ask query engine)
   (let ((str (engine query)))
     (stream-for-each (lambda (s) 
                        (if (acceptable? (state-arguments s)
                                         (state-context s)
                                         query)
                            (printf "~A~%" ((state-substitutions s) query))))
                      str)))
 
 ; ask1: statement (statement -> (stream-of argument-state)) -> void
 ; displays the first solution to a query, using the given inference engine,
 ; with any variables instantiated using the substituions of the solution state found, 
 ; or prints nothing if no solution was found.
 (define (ask1 query engine)
   (let ((str (engine query)))
     (if (not (stream-null? str))
         (let ((s (stream-car str)))
           (if (acceptable? (state-arguments s)
                            (state-context s)
                            query)
               (printf "~A~%" ((state-substitutions s) query)))))))
 
 ; success? : statement -> engine -> boolean
 ; A query is successful iff the given inference engine finds one or more argument states
 ; and the query is acceptable in the argument graph of the first argument state found.
 ;  (define (success? query) 
 ;    (lambda (engine)
 ;      (let ((s (engine query)))
 ;        (and (not (stream-null? s))
 ;             (acceptable? (state-arguments (stream-car s))
 ;                          (state-context (stream-car s))
 ;                          query)))))
 
 ;  ; failure? : statement -> engine -> boolean
 ;  ; A query fails iff the given inference engine finds one or more argument states
 ;  ; and the query is *not* acceptable in the argument graph of the first state found
 ;  (define (failure? query)
 ;    (lambda (engine)
 ;      (let ((s (engine query)))
 ;        (and (not (stream-null? s))
 ;             (not (acceptable? (state-arguments (stream-car s))
 ;                               (state-context (stream-car s))
 ;                               query))))))
 ;  
 
 ; all-acceptable? : statement engine -> boolean
 ; Checks if at least one argument graph was found by the engine and the
 ; statement is acceptable in every argument graph found.
 (define (all-acceptable? query engine)
   (let ((str (engine query)))
     (and (not (stream-null? str))
          (stream-null? (stream-filter (lambda (s)
                                         (not (acceptable? (state-arguments s)
                                                           (state-context s)
                                                           query)))
                                       str)))))
 
 ; some-acceptable? : statement engine -> boolean
 ; A statement is "defensible", relative to given engine, if it is acceptable in at least one
 ; of the argument graphs found by the engine.  
 (define (some-acceptable? query engine)
   (let ((str (engine query)))
     (not (stream-null? (stream-filter (lambda (s)
                                         (acceptable? (state-arguments s)
                                                      (state-context s)
                                                      query))
                                       str)))))
 
 ; success? : statement engine -> boolean
 ; A query succeeds iff the given engine is able to find at least one argument pro or con the statement
 ; of the query.
 (define (success? query engine) (not (stream-null? (engine query))))
 
 ; failure? : statement engine -> boolean
 ; A query fails iff the given inference engine is not able to find any arguments pro or
 ; con the statment
 (define (failure? query engine) (stream-null? (engine query)))
 
 ) ; end of shell.scm