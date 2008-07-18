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
 (carneades table)  ;; immutable, functional tables.  Keys compared with eqv?
 
 (export make-table table? insert lookup keys values  
         filter-keys filter-values 
         (rename (table-filter filter)))
 
 (import (except (rnrs) values)
         (rnrs hashtables))
 
 
 (define make-table make-eqv-hashtable)
 (define table? hashtable?)
 
 ; insert: table key value -> table
 (define (insert t1 k v) 
   (let ((t2 (hashtable-copy t1 #t)))
     (hashtable-set! t2 k v)
     (hashtable-copy t2)))
 
 ; lookup: table key default -> value 
 (define lookup hashtable-ref)
 
 (define keys hashtable-keys)
 
 ; values: table -> (list-of datum)
 (define (values t)
   (let-values (((ks vs) (hashtable-entries t)))
     (vector->list vs)))
 
 ; filter: table (-> (pair-of key value) boolean) -> (list-of (pair-of key value))
 (define (table-filter tbl pred) 
   (fold-right (lambda (k l)
                 (if (hashtable-contains? tbl k)
                     (let ((v (hashtable-ref tbl k #f)))
                       (if (pred (cons k v))
                           (cons (cons k v) l)
                           l))
                     l))
               '()
               (vector->list (hashtable-keys tbl))))
 
 (define (filter-keys pred table)
   (map car (table-filter pred table)))
 
 (define (filter-values pred table)
   (map cdr (table-filter pred table)))
 
 ) ; end of table library