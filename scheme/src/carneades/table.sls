#!r6rs

(library 
 (carneades table)  
 
 ;; Immutable, functional tables. Simple implementation using association lists. 
 ;; Keys compared with equal? by default
 
 (export make-table make-eq-table table? insert lookup keys objects  
         filter-keys
         )
 
 (import (rnrs))
 
 (define (make-table . l)
   (if (null? l)
       (make-hashtable equal-hash equal?)
       (if (= (length l) 1)
           (fold-left insert (make-hashtable equal-hash equal?) (car l))
           (if (= (length l) 3)               
               (fold-left insert (make-hashtable (car l) (cadr l)) (caddr l))
               (error "make-table: 0, 1 or 3 arguments expected!" l)))))
 
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
 
 (define (filter-keys pred t)
   (let ((l (hashtable->alist t)))
     (map car (filter pred l))))
              
 
 )