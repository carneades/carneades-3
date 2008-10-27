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
 (carneades table)  
 
 ;; Immutable, functional tables. Simple implementation using association lists. 
 ;; Keys compared with equal?
 
 (export make-table table? insert lookup keys objects  
         filter-keys filter-objects 
         (rename (table-filter filter)))
 
 (import (rnrs)
         (prefix (carneades set) set:))
 
 (define-record-type table 
   (fields pairs) ; alist of (key,value) pairs
   (protocol (lambda (new) 
               (case-lambda 
                (() (new '()))
                ((alist) (new alist))))))
                           
 
 ; insert: table key value -> table
 (define (insert t1 k v) 
   (make-table (cons (cons k v) (table-pairs t1))))
 
 ; lookup: table key default -> value 
 (define (lookup t1 k v) 
   (let ((p (assoc k (table-pairs t1))))
     (if p (cdr p) v)))
 
 (define (keys t1) 
   ; use list->set to remove duplicate keys
   (set:set->list ((set:list->set equal?) (map car (table-pairs t1)))))
 
 ; objects table -> (list-of datum)
 (define (objects t1)
   (map (lambda (k) (lookup t1 k #f)) (keys t1)))
 
 ; filter: table (-> (pair-of key value) boolean) -> (list-of (pair-of key value))
 (define (table-filter pred tbl) 
   (filter pred (map (lambda (k)
                       (cons k (lookup tbl k #f)))
                     (keys tbl))))
 
 (define (filter-keys pred table)
   (map car (table-filter pred table)))
 
 (define (filter-objects pred table)
   (map cdr (table-filter pred table)))
 
 ) ; end of table library