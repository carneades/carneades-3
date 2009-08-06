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
 (carneades statement)
 
 (export variable? constant? statement? statement=?
         statement-compare statement-positive?
         statement-negative? statement-complement statement-atom
         statement-predicate statement-formatted statement-wff
         make-fatom fatom? fatom-form fatom-term
         term? term=? compound-term?  term-functor term-args ground?
         statement-hash variables)
 
 (import (rnrs)
         (carneades base)
         (carneades lib srfi format)
         (only (carneades lib srfi strings) string-join)
         (prefix (carneades table) table:)
         (prefix (carneades lib srfi compare) compare:)
         (prefix (carneades set) set:))
 
 ; variable? : object -> boolean
 ; A logic variable is represented as a symbol prefixed with a 
 ; question mark.  For example: '?x
 (define variable? 
   (lambda (u)
     (and (symbol? u)
          (let ((s (symbol->string u)))
            (and (> (string-length s) 0)
                 (eq? (string-ref s 0) #\?))))))
 
 ; constant? : object -> boolean
 (define constant?
   (lambda (u)
     (or (and (symbol? u) (not (variable? u)))
         (number? u)
         (string? u)
         (boolean? u))))
 
 ; <term> := <atom> |  
 ; <atom> := <symbol> | <list> | <string> | <fatom>
 ; <statement> := <atom> | (not <atom>)   ;; i.e. literals
 
 ; fatom: formatted atomic formulas of predicate logic.
 (define-record-type fatom 
   (fields
    form ; string, with ~a, as in format, used to denote fields
    term ; term
    ))
 
 (define (compound-term? t1)
   (or (pair? t1)
       (fatom? t1)))
 
 ; term?: datum -> boolean
 (define (term? t1)
   (or (variable? t1)
       (constant? t1)
       (compound-term? t1)))
 
 ; term-functor -> symbol | #f
 (define (term-functor t1)
   (cond ((pair? t1)
          (car t1))
         ((fatom? t1)
          (car (fatom-term t1)))
         (else #f)))
 
 ; term-args: term -> (list-of term) 
 (define (term-args t1)
   (cond ((pair? t1)(cdr t1))
         ((fatom? t1) (cdr (fatom-term t1)))
         (else '())))
 
 (define (term=? t1 t2)
   (cond ((and (variable? t1) (variable? t2))
          (eq? t1 t2))
         ((and (constant? t1) (constant? t2))
          (equal? t1 t2))
         ((and (compound-term? t1) (compound-term? t2))
          (and (eq? (term-functor t1) (term-functor t2))
               (= (length (term-args t1)) (length (term-args t2)))
               (for-all term=? (term-args t1) (term-args t2))))
         (else #f)))
  
 
 ; term-formatted: term -> string
 ; to quote statements
 (define (term-formatted t1)
   (cond ((variable? t1) (symbol->string t1))
         ((constant? t1) (call-with-values open-string-output-port 
                                           (lambda (p e) 
                                             (write t1 p)
                                             (e))))
         ((pair? t1)
          (string-join (cons (term-formatted (car t1))
                             (map term-formatted (cdr t1)))
                       " "))
         ((fatom? t1) 
          (string-append "\"" (statement-formatted t1) "\""))))
   
 
 ; ground?: term -> boolean
 (define (ground? trm)
   (cond ((variable? trm) #f)
         ((constant? trm) #t)
         ((compound-term? trm) 
          (and (ground? (term-functor trm))
               (ground? (term-args trm))))
         (else #t)))
 
 ; variables: term -> (list-of symbol)
 ; returns a list of the variables in the term
 (define (variables trm)
   (define (f trm)
     (cond ((variable? trm) (list trm))
           ((constant? trm) null)
           ((compound-term? trm) 
            (append (f (term-functor trm))
                    (f (term-args trm))))
           (else null)))
   (set:set->list ((set:list->set eq?) (f trm))))
 
 ; example: (make-fatom "The mother of ~a is ~a." '(mother Tom Gloria))
 
 (define (statement? s1)
   (or (symbol? s1) 
       (string? s1)
       (pair? s1)
       (fatom? s1)))
 
 (define statement=? term=?)
 
; (define (statement=? s1 s2) 
;   (cond ((and (statement-positive? s1)
;               (statement-positive? s2))
;          (cond ((and (symbol? s1) (symbol? s2))
;                 (eq? s1 s2))
;                ((and (string? s1) (string? s2))
;                 (string=? s1 s2))
;                ((and (pair? s1) (pair? s2))
;                 (term=? s1 s2))
;                ((and (fatom? s1) (fatom? s2))
;                 (term=? (fatom-term s1) (fatom-term s2)))
;                ((and (pair? s1) (fatom? s2))
;                 (term=? s1 (fatom-term s2)))
;                ((and (fatom? s1) (pair? s2))
;                 (term=? (fatom-term s1) s2))
;                (else #f)))
;         ((and (statement-negative? s1)
;               (statement-negative? s2))
;          (statement=? (statement-atom s1)
;                       (statement-atom s2)))
;         (else #f)))

 
 ; statement-compare: statement statement -> {-1,0,1}
 (define (statement-compare s1 s2)
   (cond ((and (statement-positive? s1)
               (statement-positive? s2))
          (cond ((and (fatom? s1) (fatom? s2))
                 (compare:default-compare (fatom-term s1) 
                                          (fatom-term s2)))
                ((and (pair? s1) (fatom? s2))
                 (compare:default-compare s1 (fatom-term s2)))
                ((and (fatom? s1) (pair? s2))
                 (compare:default-compare (fatom-term s1) s2))
                (else (compare:default-compare s1 s2))))
         ((and (statement-negative? s1)
               (statement-negative? s2))
            (statement-compare (statement-atom s1)
                       (statement-atom s2)))
         ((and (statement-positive? s1)
               (statement-negative? s2))
          1)
         ((and (statement-negative? s1)
               (statement-positive? s2))
          -1)))
 
 (define (statement-positive? s1)
   (or (string? s1) 
       (symbol? s1)
       (fatom? s1)
       (and (pair? s1)
            (not (eq? (car s1) 'not)))))
 
 (define (statement-negative? s1)
   (and (pair? s1)
        (eq? (car s1) 'not)))
 
 (define (statement-complement s1)
   (if (pair? s1)
       (if (eq? (car s1) 'not)
           (cadr s1)   
           (list 'not s1))
       (list 'not s1)))
 
 ; statement-atom: statement -> statement
 ; The atom of the statement, i.e. strips the negation operator
 ; if there is one. 
 (define (statement-atom s1)
   (if (pair? s1)
       (if (eq? (car s1) 'not)
           (cadr s1)
           s1)
       s1))
 
 ; statement-predicate: statement -> symbol | #f
 (define (statement-predicate s1)
   (let ((s2 (statement-atom s1)))
     (cond ((pair? s2)
            (car s2))
           ((fatom? s2)
            (car (fatom-term s2)))
           (else #f))))
 
 ; statement-wff: statement -> symbol | string | list
 (define (statement-wff s1)
   (if (statement-positive? s1)
       (if (fatom? s1)
           (fatom-term s1)
           s1)
       `(not ,(statement-wff (statement-atom s1)))))
 
 ; statement-formatted: statement -> string
 (define (statement-formatted s1)
   (cond ((string? s1) s1)
         ((symbol? s1) (symbol->string s1))
         ((fatom? s1) 
          (apply format `(,(fatom-form s1) 
                          ,@(map term-formatted (cdr (fatom-term s1))))))
         ((pair? s1)
          (string-join (map term-formatted s1) ": "))))
 
 (define (statement-hash s) 
   (abs (cond ((string? s) (string-hash s))
              ((symbol? s) (symbol-hash s))
              (else (symbol-hash (statement-predicate s))))))
 
 ) ; end of statement library
