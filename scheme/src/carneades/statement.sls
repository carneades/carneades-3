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
 
 (export statement-equal? statement-compare statement-positive?
         statement-negative? statement-complement statement-atom summarize
;         statement->sxml index-by-statement index-by-id term->sxml
;         make-text text? text-id text-statement text-src text-summary
         )
 
 (import (rnrs base)
         (rnrs io simple)
         (rnrs io ports)
         (rnrs records syntactic (6))
         ; (rnrs hashtables (6))
         (prefix (carneades table) table:)
         (prefix (carneades lib srfi compare) compare:))
  
 ; <atom> := <symbol> | <list> | <string>
 ; <statement> := <atom> | (not <atom>)   ;; i.e. literals

; (define-record-type text
;   (fields
;    id       ; symbol
;    exp      ; statement | null
;    summary  ; string, quoting or paraphrasing the source text
;    src      ; string, a URL pointing to the original text
;    ))
; 
; (define (text-statement txt)
;   (if (null? (text-exp txt))
;       (text-id txt)
;       (text-exp txt)))
 
 (define statement-equal? equal?)
 

 ; statement-compare: statement statement -> {-1,0,1}
 (define statement-compare compare:default-compare)
 
 (define (statement-positive? s1)
   (or (not (pair? s1))
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
 
; (define (term->sxml t)
;   (cond ((symbol? t) (string-append (symbol->string t) " "))
;         ((list? t) (statement->sxml t))
;         (else t)))
; 
; (define (statement->sxml s)
;   (cond ((symbol? s) (list 's (symbol->string s)))
;         ((list? s)
;          (if (eq? (car s) 'not)
;              (cons 'not (list (statement->sxml (cadr s))))
;              (cons 's (map term->sxml s))))
;         (else s)))
 
 #| index-by-statement: (list-of text) -> (hashtable-of datum text)
            assumption: the statement of each text in the list is unique.  |#
; (define (index-by-statement texts)
;   (let ((tbl (table:make-table)))
;     (for-each (lambda (txt) 
;                 (if (text-statement txt)
;                     (set! tbl (table:insert tbl (text-statement txt) txt))))
;               texts)
;     tbl))
 
 #| index-by-id: (list-of text) -> (hashtable-of symbol text)
            assumption: the id of each text in the list is unique. |#
; (define (index-by-id texts)
;   (let ((tbl (table:make-table)))
;     (for-each (lambda (txt) 
;                 (set! tbl (table:insert tbl (text-id txt) txt)))
;               texts)
;     tbl))
 
 #| summarize: statement -> string
            default string representation of statements,
            for use in reports and diagrams.  The default just displays the
            statement as an s-expression. |#
 (define (summarize statement)
   (let ((write-statement-to-port
          (lambda (p f)
            (write statement p)
            (f))))
     (call-with-values open-string-output-port write-statement-to-port)))
 
 
 ) ; end of library statement