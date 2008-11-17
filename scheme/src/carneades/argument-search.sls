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
 (carneades argument-search)
 
 ; This library applies argument generators to search for argument graphs in 
 ; which some topic statement is either acceptable or not acceptable, depending on the 
 ; viewpoint. 
 
 (export make-state initial-state state? state-topic state-viewpoint state-pro-goals 
         state-con-goals state-context state-arguments state-substitutions
         opposing-viewpoint switch-viewpoint 
         make-response response? response-argument 
         response-substitutions
         find-arguments find-best-arguments goal-state? instantiated-arguments
         make-successor-state next-goals )
 
 (import (rnrs)
         (rnrs records syntactic)
         (prefix (rnrs lists) list:)
         (prefix (carneades search) search:)
         (prefix (carneades argument) arg:)
         (carneades unify)
         (carneades stream)
         (carneades statement)
         (carneades system) )
 
 (define *debug* #f)
 
 ; opposing-viewpoint: viewpoint -> viewpoint
 (define (opposing-viewpoint vp)
   (case vp 
     ((pro) 'con)
     ((con) 'pro)))
 
 ; state for use when searching for arguments which satisfy some goal
 (define-record-type state 
   (fields topic           ; statement for the main issue or thesis
           viewpoint       ; pro | con the topic
           pro-goals       ; (list-of (list-of statement)) ; disjunctive normal form
           con-goals       ; (list-of (list-of statement)) ; disjunctive normal form
           context         ; context
           arguments))     ; argument-graph 
 
 ; display-state: state -> voide
 ; for debugging
 (define (display-state state)
   (let ((subs (state-substitutions state)))
     (display "topic: ") (display (subs (state-topic state))) (newline)
     (display "viewpoint: ") (display (state-viewpoint state)) (newline)
     (display "first pro goal: ") (display (if (null? (state-pro-goals state)) "" (subs (car (state-pro-goals state))))) (newline)
     (display "first con goal: ") (display (if (null? (state-con-goals state)) "" (subs (car (state-con-goals state))))) (newline)
     (display "number of arguments: ") (display (length (arg:list-arguments (state-arguments state)))) (newline)
     (newline)))
  
  
 ; state-substitutions: state -> substitutions
 ; For backwards compatibility
 (define (state-substitutions state)
   (arg:context-substitutions (state-context state)))
 
 (define-record-type response
   (fields substitutions   ; term -> term
           argument))      ; argument | #f
 
 (define (initial-state goal context)
   (make-state goal 
               'pro 
               (list (list goal))
               null 
               context  
               arg:empty-argument-graph))
 
 
 ; switch-viewpoint: state -> state
 (define (switch-viewpoint s)
   (make-state (state-topic s)
               (opposing-viewpoint (state-viewpoint s))
               (state-pro-goals s)
               (state-con-goals s)
               (state-context s)
               (state-arguments s)))
 
 ; instantiated-arguments: state -> argument-graph
 ; return the argument graph of the state with its variables
 ; instantiated with their values in the substitutions of the state
 (define (instantiated-arguments state)
   (arg:instantiate-argument-graph (state-arguments state)
                                   (arg:context-substitutions (state-context state))))
 
 
 ; next-goals: state -> (list-of (list-of statement)) 
 ; Returns a list representing a disjunction of a conjunction of 
 ; goal statements for the current viewpoint to try to solve in the state
 ; If no goals remain for the current viewpoint, the empty list is returned.
 (define (next-goals state)
   (case (state-viewpoint state)
     ((pro) (state-pro-goals state))
     ((con) (state-con-goals state))))
 
 ; questioned-assumptions: (list-of statement) context -> (list-of statement)
 (define (questioned-assumptions assumptions context)
   (list:filter (lambda (s) (arg:questioned? context s)) assumptions))
 
 ; update-goals: (list-of (list-of statement)) (list-of statement)  ->
 ;             (list-of (list-of statement))
 ; replaces the first literal of the first clause in a list of clauses
 ; with the given replacement statements. If the resulting clause is empty
 ; return the remaining clauses, otherwise return the result of replacing the first clause 
 ; with the updated clause.
 (define (update-goals disjunction-of-conjunctions replacements)
   (let ((clause1 (append replacements (cdr (car disjunction-of-conjunctions)))))
     (if (null? clause1)
         (cdr disjunction-of-conjunctions)
         (cons clause1 (cdr disjunction-of-conjunctions)))))
 
 ; make-successor-state: state response -> state
 (define (make-successor-state state response)
   (let ((new-subs (response-substitutions response))
         (arg (response-argument response)))
     (if arg
         (let* ((arg (response-argument response))
                (conclusion (arg:argument-conclusion arg))
                (assumptions (questioned-assumptions (map arg:premise-statement 
                                                          (list:filter arg:assumption? (arg:argument-premises arg)))
                                                     (state-context state)))
                (premises (append (map arg:premise-statement 
                                       (list:filter arg:ordinary-premise? (arg:argument-premises arg)))
                                  assumptions))
                (exceptions (map arg:premise-statement 
                                 (list:filter arg:exception? (arg:argument-premises arg)))))
           (make-state (state-topic state)
                       (state-viewpoint state)
                       ; new pro goals
                       (case (state-viewpoint state)
                         ((pro) (update-goals (state-pro-goals state)
                                              premises))
                         ((con) (append (state-pro-goals state)
                                        (map list exceptions) ; separate clause for each exception
                                        ; rebuttals and rebuttals of assumptions:
                                        ; (list (list (statement-complement conclusion)))
                                        ; (map list (map statement-complement assumptions))
                                        )))   
                       ; new con goals
                       (case (state-viewpoint state)
                         ((pro) (append (state-con-goals state)
                                        (map list exceptions) ; separate clause for each exception
                                        ; rebuttals and rebuttals of assumptions:
                                        ; (list (list (statement-complement conclusion)))
                                        ; (map list (map statement-complement assumptions))
                                        ))
                         ((con) (update-goals (state-con-goals state) 
                                              premises)))
                       ; make an issue of the conclusion:
                       (arg:update-substitutions 
                        (if (arg:decided? (state-context state) conclusion)
                            (state-context state)
                            (arg:question (state-context state) (list conclusion))) 
                        new-subs)
                       ; extend argument graph with the new arg
                       (arg:assert-arguments (state-arguments state) (list arg))))
         
         ; create a successor state by modifying the substitutions of the context, but without
         ; putting forward a new argument.
        
         (make-state (state-topic state)
                     (state-viewpoint state)
                     ; new pro goals
                     (case (state-viewpoint state)
                       ((pro) (update-goals (state-pro-goals state) '()))
                       ((con) (state-pro-goals state)))
                     ; new con goals
                     (case (state-viewpoint state)
                       ((pro) (state-con-goals state))
                       ((con) (update-goals (state-con-goals state) '())))
                     ; replace the substitutions of the context
                     (arg:update-substitutions (state-context state) new-subs)
                     ; no new arguments
                     (state-arguments state)))))
 
 
 ; type generator : statement state -> (stream-of response)
 ; A generator maps a goal statement to a stream of arguments pro this goal.  The conclusion
 ; of each argument will be a positive statement equal to the atom of the goal statement.
 ; If the goal statement is negative, the arguments generated will be con the complement of the 
 ; goal statement. If the statement is #f the generator should return an empty stream.
 
 ; make-transitions: (list-of generator) -> (node -> (stream-of node))
 ; Uses stream-flatmap to interleave the application of the argument generators.
 (define (make-transitions l)
   (define (apply-generator generator node) ; -> (stream-of node)
     (let ((state1 (search:node-state node)))
       (stream-map (lambda (state2) ; -> node
                     (search:make-node (+ (search:node-depth node) 1)
                                       #f   ; no node label
                                       node ; parent node
                                       state2))
                   (stream-map (lambda (response) ; response -> state
                                 (make-successor-state 
                                  (search:node-state node) 
                                  response)) 
                               (stream-flatmap 
                                (lambda (clause) ; -> (stream-of (pair-of argument substitutions))
                                  (let ((goal (car clause)))
                                    (generator goal state1)))
                                (list->stream (next-goals state1)))))))
   (lambda (node)
     (stream-flatmap (lambda (g) (apply-generator g node))
                     (list->stream l))))
 
 ; goal-state?: state -> boolean
 ; If viewpoint of the state is pro, then the goal is to find a
 ; state in which the topic statement is acceptable. If the viewpoint is con, 
 ; the goal is to find a state in which topic is not acceptable. 
 (define (goal-state? state)
   (let* ((in (arg:in? (state-arguments state) 
                  (state-context state)
                  (state-topic state)))
          (result (case (state-viewpoint state)
                     ((pro) in)
                     ((con) (not in)))))
     (if *debug* 
         (begin 
           (printf "goal-state: ~a~%" result)
           (display-state state)))
     result))
 
 
 ; node-compare: node node -> {-1,0,1}
 ; Prefer nodes with the smallest number of issues
 ; To do: check whether this measure produces a better strategy than
 ; simple breadth-first search.  Aren't the nodes with the fewest number of issues
 ; also the ones closest to the root of the search tree?
; (define (node-compare n1 n2)
;   (let ((s1 (search:node-state n1))
;         (s2 (search:node-state n2)))
;     (compare:default-compare (length (issues 
;                                       (state-arguments s1)
;                                       (state-context s1)))
;                              (length (issues 
;                                       (state-arguments s2)
;                                       (state-context s2))))))
 
 ; example search strategies
 ; (define strat1 (search:depth-first (search:make-resource 10)))
 ; (define strat2 ((search:best-first node-compare) (search:make-resource 10)))
 
 ; find-arguments: resource-limited-strategy resource state (list-of generator) -> (stream-of state)
 (define (find-arguments strategy r initial-state generators)
   (stream-map (lambda (node) (search:node-state node))
               (search:search (search:make-problem (search:make-root initial-state)
                                                   (make-transitions generators) 
                                                   goal-state?)
                              (strategy r))))
 
 
 ; find-best-arguments: resource-limited-strategy r int state (list-of generator) ->
 ;                       (stream-of state)
 ; find the best arguments for *both* viewpoints, starting with the viewpoint of the
 ; initial state. An argument is "best" if it survives all possible attacks from arguments 
 ; which can be constructed using the provided argument generators, within the given 
 ; search limits.  find-best-arguments allows negative conclusions to be explained, since it
 ; includes successful counterarguments in its resulting stream of arguments.  
 (define (find-best-arguments strategy r max-turns state1 generators)
   (define (loop turns str1) 
     (if (<= turns 0)
         str1
         (stream-flatmap (lambda (state2)
                           ; lookahead for attacking arguments at the next level, str2,
                           ; by taking the opposing viewpoint and searching again
                           (let ((str2 (find-arguments strategy r
                                                       (switch-viewpoint state2)
                                                       generators)))
                             (if (stream-null? str2)       ; no counterarguments found
                                 (stream state2)           ; state2 is a best argument (base case)
                                 (loop (- turns 1) str2)))); recursive case
                         str1)))
   (if (<= max-turns 0) 
       stream-null
       (loop (- max-turns 1) (find-arguments strategy r state1 generators))))
 
 
 
 
 ) ; end of module

