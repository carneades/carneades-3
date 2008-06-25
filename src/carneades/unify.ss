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
 (carneades unify)
 
 (export variable? identity unify* unify genvar rename-variables ground?)
 (import (rnrs base)
         (rnrs hashtables)
         (only (scheme base) gensym)) ; PLT Scheme dependency
 
 ; Based on the implementation of the unification algorithm from
 ; the book "The Scheme Programming Language", by Kent Dybvig.
 
 ; type term = literal | variable | list
 ; type literal = symbol | number | string | boolean
 
 ; variable? : object -> boolean
 ; A logic variable is represented as a symbol prefixed with a 
 ; question mark, as in SWRL. For example: '?x
 (define variable? 
   (lambda (u)
     (and (symbol? u)
          (let ((s (symbol->string u)))
            (and (> (string-length s) 0)
                 (eq? (string-ref s 0) #\?))))))
 
 ; literal? : object -> boolean
 ; Warning: variables are also literals. Be sure to check whether
 ; an object is a variable before checking whether it is a literal,
 ; in contexts where the two need to be distinguished.
 (define literal?
   (lambda (u)
     (or (symbol? u)
         (number? u)
         (string? u)
         (boolean? u))))
 
 
 ; occurs? returns true if and only if u occurs in v
 (define occurs?
   (lambda (u v)
     (and (pair? v)
          (let f ((l (cdr v)))
            (and (pair? l)
                 (or (eq? u (car l))
                     (occurs? u (car l))
                     (f (cdr l)))))))) 
 
 ;; substitution constructors: identity and sigma
 
 ; identity substition
 (define identity (lambda (x) x))
 
 ; sigma returns a new substitution procedure extending s by
 ; the substitution of u with v
 (define sigma
   (lambda (u v s)
     (lambda (x)
       (let f ((x (s x)))
         (cond ((variable? x) (if (eq? x u) v x))
               ((pair? x) (cons (car x) (map f (cdr x))))
               (else x))))))
 
 
 ; try-subst tries to substitute u for v but may require a
 ; full unification if (s u) is not a variable, and it may
 ; fail if it sees that u occurs in v.
 (define try-subst
   (lambda (u v s ks kf occurs-check)
     (let ((u (s u)))
       (if (not (variable? u))
           (unify* u v s ks kf occurs-check)
           (let ((v (s v)))
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
       ((and (literal? u) (literal? v))
        (if  (equal? u v) (ks s) (kf "clash")))
       ((and (pair? u) 
             (pair? v) 
             (eq? (car u) (car v))
             (= (length u) (length v)))
        (let f ((u (cdr u)) (v (cdr v)) (s s))
          (if (null? u)
              (ks s)
              (unify* (car u)
                      (car v)
                      s
                      (lambda (s) (f (cdr u) (cdr v) s))
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
              (hashtable-ref tbl trm)
              (let ((v (genvar)))
                (hashtable-set! tbl trm v)
                v)))
         ((pair? trm)
          (cons (rename-variables tbl (car trm)) (rename-variables tbl (cdr trm)))) 
         (else trm)))
 
 ; ground: term -> boolean
 (define (ground? trm)
   (cond ((variable? trm) #f)
         ((literal? trm) #t)
         ((pair? trm) (and (ground? (car trm))
                           (ground? (cdr trm))))
         (else #t)))
 
 ) ; end of unify module
