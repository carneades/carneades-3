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
 (carneades table)  ;; immutable, functional tables.  Keys compared with equal?
 
 (export make-table table? insert lookup keys objects  filter-keys filter-objects (rename (table-filter filter)))
 
 (import (rnrs)
         (prefix (only (scheme) 
                       hash? make-immutable-hash hash-set hash-ref 
                       hash-for-each for/list in-hash-keys in-hash-values
                       in-hash-pairs car cdr)
                 plt:)
         (prefix (scheme mpair) plt:))
 
 
 (define (make-table) (plt:make-immutable-hash '()))
 (define table? plt:hash?)
 
 ; insert: table key value -> table
 (define insert plt:hash-set)
 
 ; lookup: table key default -> value 
 (define lookup plt:hash-ref)
 
 (define (keys table)
   (plt:list->mlist (plt:for/list ((i (plt:in-hash-keys table))) i)))
 
 (define (objects table)
   (plt:list->mlist (plt:for/list ((i (plt:in-hash-values table))) i)))
 
 ; filter: table (-> (pair-of key value) boolean) -> (list-of (pair-of key value))
 (define (table-filter pred table) 
   (filter pred (plt:list->mlist (plt:for/list ((i (plt:in-hash-pairs table))) 
                                               (cons (plt:car i) (plt:cdr i))))))
 
 (define (filter-keys pred table)
   (map car (table-filter pred table)))
 
 (define (filter-objects pred table)
   (map cdr (table-filter pred table)))
 

 ) ; end of table library