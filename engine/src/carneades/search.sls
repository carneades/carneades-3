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
 (carneades search)
 
 (export make-node node? node-depth node-label node-parent node-state
         make-root root? make-problem problem? problem-root problem-space problem-goal
         search path make-resource resource? resource-amount resource-empty?
         use depth-first breadth-first iterative-deepening best-first)
 
 (import (rnrs)
         (rnrs records syntactic)
         (carneades stream)
         (prefix (carneades heap) heap:))
 
 (define-record-type node (fields depth label parent state))
  
 ; make-root: state -> node
 (define (make-root s) (make-node 0 #f #f s))
 
 ; root?: node -> boolean
 (define (root? n) (not (node-parent n)))
 
 ; The problem-space is a function of type (node -> (stream-of node)),
 ; where the depth of each of the children of the node expanded, p,
 ; is (+ (node-depth p) 1) 
 (define-record-type problem 
   (fields root     ; node
           space    ; node -> (stream-of node)  
           goal))   ; state -> boolean
 
 ; type strategy = problem -> (stream-of node)
 ; type resource-limited-strategy = resource -> strategy
 
 ; search: problem strategy  -> (stream-of node)
 (define (search p s) (s p))
 
 ; expand-node: node problem -> (stream-of node)
 (define (expand-node n p) 
   ; (printf "label:~a; depth:~a~n" (node-label n) (node-depth n))  ; for debugging
   ((problem-space p) n))
 
 ; path : node -> (list-of label)
 (define (path n) 
   (if (root? n) 
       '() 
       (append (path (node-parent n)) 
               (list (node-label n)))))
 
 (define-record-type resource (fields (mutable amount)))
 (define (resource-empty? r) (<= (resource-amount r) 0))
 (define (use r) 
   ; (printf ".") ; debug
   (if (> (resource-amount r) 0)
       (resource-amount-set! r (- (resource-amount r) 1))))
 
 ; depth-first: resource-limited-strategy
 ; resource: the maximum number of nodes which may be expanded
 (define (depth-first r)
   (lambda (p)
     (define (loop open-nodes)
       (use r)
       (if (or (stream-null? open-nodes)(resource-empty? r))
           stream-null
           (let ((node (stream-car open-nodes)))
             (cond (((problem-goal p) (node-state node))
                    (stream-cons node (loop (stream-cdr open-nodes))))
                   (else (loop (stream-append (expand-node node p) 
                                              (stream-cdr open-nodes))))))))
     (loop (stream (problem-root p)))))
 
 ; breadth-first: resource-limited-strategy
 ; resource: the maximum number of nodes which may be expanded
 (define (breadth-first r)
   (lambda (p) 
     (define (loop open-nodes)
       (use r)
       (if (or (stream-null? open-nodes) (resource-empty? r))
           stream-null
           (let ((node (stream-car open-nodes)))
             (cond (((problem-goal p) (node-state node))
                    (stream-cons node (loop (stream-cdr open-nodes))))
                   (else (loop (stream-append (stream-cdr open-nodes) 
                                              (expand-node node p))))))))
     (loop (stream (problem-root p)))))
 
 ;   iterative-deepening: integer integer -> resource-limited-strategy
 ;   init: the initial depth to search
 ;   step: how much to increase the depth on each iteration
 ;   resource: the maximum number of nodes which may be expanded (not a depth limit)
 (define (iterative-deepening init step)
   (lambda (r)
     (define (depth-first depth-limit)
       (lambda (p)
         (define (loop open-nodes)
           (use r) 
           (if (or (stream-null? open-nodes)
                   (resource-empty? r))
               stream-null
               (let ((n (stream-car open-nodes)))
                 (cond (((problem-goal p) (node-state n))
                        (stream-cons n (loop (stream-cdr open-nodes))))
                       ((<= (node-depth n) depth-limit)
                        (loop (stream-append (expand-node n p) 
                                             (stream-cdr open-nodes))))
                       (else                  
                        (loop (stream-cdr open-nodes)))))))
         (loop (stream (problem-root p)))))
     (lambda (p) 
       (if (resource-empty? r)
           stream-null
           (let ((x ((depth-first init) p)))
             (stream-append x
                            (((iterative-deepening (+ init step) step) r) p)))))))
 
 
 ; best-first: (node node -> {-1,0,1}) -> resource-limited-strategy
 ; resource: the maximum number of nodes which may be expanded
 (define (best-first compare)
   (lambda (r)
     (lambda (p)
       (define (loop open-nodes)
         (use r)
         (if (or (heap:empty? open-nodes) (resource-empty? r))
             stream-null
             (let ((node (heap:find-min open-nodes)))
               (cond (((problem-goal p) (node-state node))
                      (stream-cons node (loop (heap:delete-min node open-nodes))))
                     (else
                      (loop (heap:merge (heap:list->heap compare (stream->list (expand-node node p)))
                                        (heap:delete-min node open-nodes))))))))
       (loop (heap:singleton compare (problem-root p))))))
 
 ) ; end module search