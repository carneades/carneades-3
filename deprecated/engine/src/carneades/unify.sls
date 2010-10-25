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
 (carneades unify2)
 
 (export variable? identity unify* unify genvar rename-variables ground? apply-substitution)
 (import (rnrs base)
         (rnrs hashtables)
         (carneades statement)
         (only (carneades system) gensym)
         (prefix (racket base) rkt:))
 
 ; Based on the implementation of the unification algorithm from
 ; the book "The Scheme Programming Language", by Kent Dybvig.
 
 
 ; occurs? returns true if and only if u occurs in v
 (define occurs?
   (lambda (u v)
     (and (compound-term? v)
          (let f ((l (term-args v)))
            (and (compound-term? l)
                 (or (eq? u (term-functor l))
                     (occurs? u (term-functor l))
                     (f (term-args l)))))))) 
 
 (define (apply-substitution s t)
   (let ((t2 (cond ((variable? t) (rkt:hash-ref s t t))
                   ((pair? t) 
                    (cons (car t) (map (lambda (x) (apply-substitution s x)) (cdr t))))
                   ((fatom? t) 
                    (make-fatom (fatom-form t)
                                (cons (car (fatom-term t))
                                      (map (lambda (x) (apply-substitution s x)) (cdr (fatom-term t))))))
                   (else t))))
     (if (and (variable? t2)
              (rkt:hash-has-key? s t2))
         (apply-substitution s t2)
         t2)))
   
   
 
 ;; substitution constructors: identity and sigma
 
 ; identity substition
 (define identity 
   (rkt:make-immutable-hasheq '()))
 
 ; sigma returns a new substitution procedure extending s by
 ; the substitution of u with v
 (define sigma
   (lambda (u v s)
     (rkt:hash-set s u v )))
     
 
 ; try-subst tries to substitute u for v but may require a
 ; full unification if (s u) is not a variable, and it may
 ; fail if it sees that u occurs in v.
 (define try-subst
   (lambda (u v s ks kf occurs-check)
     (let ((u (apply-substitution s u)))
       (if (not (variable? u))
           (unify* u v s ks kf occurs-check)
           (let ((v (apply-substitution s v)))
             (cond
               ((eq? u v) (ks s))
               ((and occurs-check (occurs? u v)) (kf "cycle"))
               (else (ks (sigma u v s))))))))) 
 
 ; unify* attempts to unify u and v with a continuation-passing
 ; style that returns a substitution to the success argument
 ; ks or an error message to the failure argument kf.  The
 ; substitution itself is represented by a procedure from
 ; variables to terms. The occurs-check flag determines whether
 ; the occurs check is performed.
 (define unify*
   (lambda (u v s ks kf occurs-check)
     (cond
       ((variable? u) (try-subst u v s ks kf occurs-check))
       ((variable? v) (try-subst v u s ks kf occurs-check))
       ((and (constant? u) (constant? v))
        (if  (equal? u v) (ks s) (kf "clash")))
       ((and (compound-term? u) 
             (compound-term? v) 
             (eq? (term-functor u) (term-functor v))
             (= (length (term-args u)) (length (term-args v))))
        (let f ((u (term-args u)) (v (term-args v)) (s s))
          (if (null? u)
              (ks s)
              (unify* (car u)
                      (car v)
                      s
                      (lambda (s) (f (term-args u) (term-args v) s))
                      kf
                      occurs-check))))
       (else (kf "clash"))))) 
 
 ; unify: term term -> substitution | #f
 ; unify is a simplified interface to unify*, where the initial
 ; substitution is the identity procedure, the initial success
 ; continuation returns the unified term, the initial failure
 ; continuation returns #f and the occurs-check is not performed.
 (define unify
   (lambda (u v)
     (unify* u
             v
             identity
             (lambda (s) s)
             (lambda (msg) #f)
             #f)))
 
 ; genvar: -> variable
 ; generate a fresh, unique variable
 (define (genvar) (gensym "?"))
 
 ; rename: hashtable term -> term
 ; Systematically rename the variables in term, keeping track of the 
 ; replacements in the hash table. Warning: this can have side-effects. 
 ; The hashtable can be modified.
 (define (rename-variables tbl trm)
   (cond ((variable? trm) 
          (if (hashtable-contains? tbl trm)
              (hashtable-ref tbl trm #f)
              (let ((v (genvar)))
                (hashtable-set! tbl trm v)
                v)))
         ((pair? trm)
          (cons (rename-variables tbl (car trm)) 
                (rename-variables tbl (cdr trm))))
         ((fatom? trm)
          (make-fatom (fatom-form trm)
                      (cons (rename-variables tbl (car (fatom-term trm)))
                            (rename-variables tbl (cdr (fatom-term trm))))))
         (else trm)))
 
 ) ; end of unify module
