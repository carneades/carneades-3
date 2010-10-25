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
 
 (import (rnrs)
         (prefix (racket base) rkt:)
         (racket dict)
         (racket mpair))
 
 ; (make-table hash-func equiv? alist)
 (define (make-table hf eq alist)
   (fold-left (lambda (h p) (dict-set h (car p) (cdr p))) (make-immutable-custom-hash eq hf) alist))
               
 
 ; (make-eq-table alist)
 (define (make-eq-table l)
   (rkt:make-immutable-hasheq (mlist->list (map (lambda (p) (rkt:cons (car p) (cdr p))) l))))
   
 (define (table? t)
   (or (rkt:hash? t)
       (dict? t)))
 
 (define (insert ht k v)
   (cond
     ((rkt:hash? ht) (rkt:hash-set ht k v))
     ((dict? ht) (dict-set ht k v))
     (else (error "table-insert: no table found" ht))))
  
 (define (lookup ht k f)   
   (cond
     ((rkt:hash? ht) (rkt:hash-ref ht k f))
     ((dict? ht) (dict-ref ht k f))
     (else (error "table-lookup: no table found" ht))))
 
 (define (get-hash-keys* h pos)
   (let ((next (rkt:hash-iterate-next h pos)))
     (if next
         (cons (rkt:hash-iterate-key h pos) (get-hash-keys* h next))
         (list (rkt:hash-iterate-key h pos)))))
 
 (define (get-hash-keys h)
   (let ((first (rkt:hash-iterate-first h)))
     (if first         
         (get-hash-keys* h first)
         '())))
 
 (define (get-hash-values* h pos)
   (let ((next (rkt:hash-iterate-next h pos)))
     (if next
         (cons (rkt:hash-iterate-value h pos) (get-hash-values* h next))
         (list (rkt:hash-iterate-value h pos)))))
 
 (define (get-hash-values h)
   (let ((first (rkt:hash-iterate-first h)))
     (if first         
         (get-hash-values* h first)
         '())))
 
 (define (get-hash-pairs* h pos)
   (let ((next (rkt:hash-iterate-next h pos))
         (k (rkt:hash-iterate-key h pos))
         (v (rkt:hash-iterate-value h pos)))
     (if next
         (cons (cons k v) (get-hash-pairs* h next))
         (list (cons k v)))))
 
 (define (get-hash-pairs h)
   (let ((first (rkt:hash-iterate-first h)))
     (if first         
         (get-hash-pairs* h first)
         '())))
 
 (define (keys ht)
   (cond
     ((rkt:hash? ht) (get-hash-keys ht))
     ((dict? ht) (list->mlist (rkt:for/list ([k (in-dict-keys ht)]) k)))
     (else (error "table-keys: no table found" ht))))
 
 (define (objects ht)
   (cond
     ((rkt:hash? ht) (get-hash-values ht))
     ((dict? ht) (list->mlist (rkt:for/list ([v (in-dict-values ht)]) v)))
     (else (error "table-keys: no table found" ht))))
 
 (define (hashtable->alist ht)
   (cond
     ((rkt:hash? ht) (get-hash-pairs ht))
     ((dict? ht) (list->mlist (rkt:for/list ([p (in-dict-pairs ht)]) (cons (rkt:car p) (rkt:cdr p)))))
     (else (error "table-keys: no table found" ht))))

              
  (define (filter-keys pred t)
   (let ((l (hashtable->alist t)))
     (map car (filter pred l))))
              
 )