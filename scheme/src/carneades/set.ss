;;;;
;;;; Functional, immutable sets, given an equality predicate on elements.
;;;;
;;;; Represented as an unordered list without duplicates.
;;;;
;;;; This code is in the public domain.
;;;; 
;;;; Darius Bacon <darius@accesscom.com>
;;;; http://www.accesscom.com/~darius
;;;; 
;;;; Ported to R6RS by Stefan Ballnat.

#!r6rs

(library
 (carneades set)
 
 (export singleton list->set set->list empty? select
         (rename (set-filter filter) 
                 (set-any? any?) 
                 (set-every? every?)
                 (set/finder finder)
                 (set/foldl fold-left)
                 (set-empty empty-set)
                 (set-adjoin adjoin)
                 (set-union union)
                 (set-intersection intersection)
                 (set-disjoint? disjoint?)
                 (set-difference difference)
                 (set-subset? subset?)))
 
 
 (import (rnrs))
 
 ;; Return an equivalent of MEMBER that compares using =?.
 (define set/finder
   (lambda (=?)
     (cond ((eq? =? eq?) memq)
           ((eq? =? eqv?) memv)
           ((eq? =? equal?) member)
           (else 
            (lambda (x xs)
              (let searching ((xs xs))
                (cond ((null? xs) #f)
                      ((=? x (car xs)) xs)
                      (else (searching (cdr xs))))))))))
 
 ;; This should be inlined below for speed...
 (define set/foldl
   (lambda (fn id ls)
     (let folding ((id id) (ls ls))
       (if (null? ls)
           id
           (folding (fn (car ls) id)
                    (cdr ls))))))
 
 
 ;; The empty set.
 (define set-empty (lambda (=?) '()))
 
 ;; Return a set containing one element, X.
 (define singleton
   (lambda (=?)
     (lambda (x)
       (list x))))
 
 ;; Return a set containing the members of X-LIST.
 (define list->set
   (lambda (=?)
     (let ((adjoin (set-adjoin =?)))
       (lambda (x-list)
         (set/foldl adjoin (set-empty =?) x-list)))))
 
 ;; The set containing X and the elements of XS.
 (define set-adjoin
   (lambda (=?)
     (let ((mem= (set/finder =?)))
       (lambda (x xs)
         (if (mem= x xs)
             xs
             (cons x xs))))))
 
 ;; The elements in either of XS or YS.
 (define set-union
   (lambda (=?)
     (let ((adjoin (set-adjoin =?)))
       (lambda (xs ys)
         (set/foldl adjoin ys xs)))))
 
 (define (union s1 s2)
   ((set-union eq?) s1 s2))
 
 ;; The elements in both of XS and YS.
 (define set-intersection
   (lambda (=?)
     (let ((mem= (set/finder =?)))
       (lambda (xs ys)
         (set/foldl (lambda (x zs)
                      (if (mem= x ys) 
                          (cons x zs)
                          zs))
                    '()
                    xs)))))
 
 
 ;; Return true iff XS and YS have no elements in common.
 (define set-disjoint?
   (lambda (=?)
     (let ((mem= (set/finder =?)))
       (lambda (xs ys)
         (not (set-any? (lambda (x) (mem= x ys))))))))
 
 (define (disjoint? s1 s2)
   ((disjoint? eq?) s1 s2)) 
 
 ;; The elements in XS but not YS.
 (define set-difference
   (lambda (=?)
     (let ((mem= (set/finder =?)))
       (lambda (xs ys)
         (set/foldl (lambda (x zs)
                      (if (mem= x ys) 
                          zs
                          (cons x zs)))
                    '()
                    xs)))))
 
 ;; Return true iff XS is a subset of YS.
 (define set-subset?
   (lambda (=?)
     (let ((mem= (set/finder =?)))
       (lambda (xs ys)
         (set-every? (lambda (x) (mem= x ys))
                     xs)))))
 
 
 ;; Return true iff (TEST? X) is true for some X in XS.
 (define set-any?
   (lambda (test? xs)
     (let checking ((xs xs))
       (if (null? xs)
           #f
           (or (test? (car xs))
               (checking (cdr xs)))))))
 
 
 ;; Return true iff (TEST? X) is true for every X in XS.
 (define set-every?
   (lambda (test? xs)
     (let checking ((xs xs))
       (if (null? xs)
           #t
           (and (test? (car xs))
                (checking (cdr xs)))))))
 
 ;; Return a list of the elements in XS.
 (define set->list
   (lambda (xs) xs))
 
 (define set-filter filter)
 
 (define (empty? s)
   (equal? s '()))
 
 (define (select s)
   (if (empty? s)
       (error 'select "empty list!")
       (car s)))
 
 ; (define-syntax make-eq
 ;   (lambda (x)
 ;     (syntax-case x ()
 ;       ((_) #'set-empty)
 ;       ((_ elem) #'((singleton eq?) elem))
 ;       ((_ e1 e2) #'(list->eq (list e1 e2)))
 ;       ((_ e1 e2 ...) #'(list->eq (list e1 e2 ...))))))
 
 
 ) ; end of set