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

;;; Ported to R6RS by Stefan Ballnat;  Based on the heap module of the 
;;; Schematics Cookbook <http://schemecookbook.org/view/Cookbook/FunctionalHeap>

#!r6rs

(library
 (heap)

 (export make-empty empty? singleton list->heap find-min merge insert 
         merge-heap-pairs delete-min)
 

 (import (rnrs base)
         (rnrs lists)
         (rnrs records syntactic (6))
         (rnrs control (6)))

 ; for first, second and rest
 ; A PAIRING HEAP is either
 ;    (make-heap <= '() )
 ; or
 ;    (make-heap <= (make-node elm heaps))
 ; where <= is an order on the elements, elm is an element,
 ; and heaps is a list of heap-ordered trees.
 
 (define-record-type heap (fields <= node-or-empty))
 (define-record-type node (fields elm heaps))
 
 ; heaps : heap -> (list heap)
 ;  given a non-empty heap H return the list of subheaps
 ;  (for internal use)
 (define (heaps H)
   (node-heaps (heap-node-or-empty H)))
 
 ; make-empty : (any (-> (any any) boolean)) -> heap
 ; return an empty heap with <= as element order
 (define (make-empty <=)
   (make-heap <= '()))
 
 ; empty? : heap -> boolean
 ;  is the heap H empty?
 (define (empty? H)
   (and (heap? H)
        (null? (heap-node-or-empty H))))
 
 ; singleton: any (-> (any any) boolean) -> heap
 (define (singleton <= x) 
   (insert x (make-empty <=)))
 
 ; list->heap: (-> (-> (any any) boolean)) 
 ;                 (list-of any)
 ;                 -> heap
 (define (list->heap <= l)
   (fold-right (lambda (item heap) (insert heap item))
               (make-empty <=)
               l))
                   
 
 ; find-min : heap -> element
 ;  return the smallest element of the heap H with relation to
 ;  the heap order <=
 (define (find-min H) 
   (when (empty? H)
     (error "find-min: An empty heap has no root; given " H))
   (node-elm (heap-node-or-empty H)))
 
 ; merge : heap heap -> heap
 (define (merge H1 H2)
   ; return a heap holding the elements of both H1 and H2
   (let ([<= (heap-<= H1)])
     (cond
       [(empty? H1)  H2]
       [(empty? H2)  H1]
       [else         (let ([x (find-min H1)]
                           [y (find-min H2)])
                       (if (<= x y) 
                           (make-heap <= (make-node x (cons H2 (heaps H1))))
                           (make-heap <= (make-node y (cons H1 (heaps H2))))))])))
 
 ; insert : element heap -> heap
 ;  return a new heap holding the elements of the heap H and the element x
 (define (insert H x)
   (merge (make-heap (heap-<= H) (make-node x '()))
          H))
 
 ; merge-heap-pairs : (list heap) -> heap
 ;  return a new heap holding all the elements of the heaps in the list
 (define (merge-heap-pairs <= hs)
   (cond
     [(null? hs) (make-empty <=)]
     [(null? (cdr hs)) (car hs)]
     [else (merge (merge (car hs) (cadr hs))
                  (merge-heap-pairs <= (cdr (cdr hs))))]))
 
 ; delete-min : heap -> heap
 ;  return a new heap holding all the elements of H but the smallest
 (define (delete-min H)
   (make-heap (heap-<= H)
              (heap-node-or-empty (merge-heap-pairs (heap-<= H) (heaps H)))))
 
 ) ; end of heap library
