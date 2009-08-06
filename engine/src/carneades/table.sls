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
 
 ;; Functional tables. Simple implementation using r6rs mutable hashtables
 
 (export make-table make-eq-table table? insert lookup keys objects  
         filter-keys
         )
 
 (import (rnrs))
 
 ; (make-table hash-func equiv? alist)
 (define (make-table hf eq alist)
   (fold-left insert (make-hashtable hf eq) alist))
               
 
 ; (make-eq-table alist)
 (define (make-eq-table l)
   (fold-left insert (make-eq-hashtable) l))
   
 (define table? hashtable?)
 
 (define (insert ht k v)
   (let* ((h (hashtable-copy ht #t)))
     (hashtable-set! h k v)
     h))
  
 (define lookup hashtable-ref)
 
 (define (keys ht)
   (vector->list (hashtable-keys ht)))
 
 (define (objects ht)
   (call-with-values (lambda () (hashtable-entries ht))
                     (lambda (k e) (vector->list e))))
 
 (define (hashtable->alist t)
   (call-with-values (lambda () (hashtable-entries t))
                     (lambda (k e) (map cons (vector->list k) (vector->list e)))))
 
 (define (filter-keys2 pred t)
   (let ((l (hashtable->alist t)))
     (map car (filter pred l))))
 
 #;(define (filter-keys pred t)
   (let ((result '()))
     (call-with-values (lamda () (hashtable-entries ht))
                       (lambda (k v)
                         (vector-for-each (lambda (key val)
                                            (if (pred (key . val))
                                                (set! result (cons key result))))
                                          k v)))
     result))
              
  (define (filter-keys pred t)
   (let ((result '()))
     (let*-values (((k v) (hashtable-entries t)))
       (vector-for-each (lambda (key val)
                          (if (pred (cons key val))
                              (set! result (cons key result))))
                        k v))
     result))
              
 )