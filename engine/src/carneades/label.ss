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

; dnf = (list-of (list-of statement)) - i.g. (A or (B and C)) = ((A) (B C))

(library
 (carneades label)
 
 (export statement-in-label statement-out-label
         argument-in-label argument-out-label)
 
 (import (rnrs)
         (carneades argument)
         (carneades statement))
 
 
 (define (flatmap f l) (apply append (map f l))) 
 
 ; combine-conjunction-of-dnf: (list-of dnf) -> dnf
 (define (combine-conjunction-of-dnf c)
   (cond
     ((null? c) '())
     ((null? (cdr c)) (car c))
     ((conjunct-dnf-with-dnf (car c) (combine-conjunction-of-dnf (cdr c))))))
 
 ; conjunct-dnf-with-dnf: dnf dnf -> dbf
 (define (conjunct-dnf-with-dnf d1 d2)   
   (flatmap (lambda (c) (conjunct-clause-with-dnf c d2)) d1))
 
 ; conjunct-clause-with-dnf (list-of statement) dnf -> dnf
 (define (conjunct-clause-with-dnf cl d)
   (map (lambda (c) (apply append (list cl c))) d))
 
 (define (true-filter l)
   (remp (lambda (a) (equal? a #t)) l))
 
 ; argument-in-label: argument-graph argument -> dnf
 (define (argument-in-label ag asm arg)
   (let*-values (((ex pr) (partition exception? (argument-premises arg)))
                 ((pr-labels) (let ((l (map (lambda (p) (statement-in-label ag asm (premise-statement p))) pr)))
                                (if (or (for-all (lambda (l) (equal? l (list (list #t)))) l)
                                        (null? l))
                                    #t
                                    l)))
                 ((ex-labels) (let ((l (map (lambda (p) (statement-out-label ag asm (premise-statement p))) ex)))
                                (if (or (for-all (lambda (l) (equal? l (list (list #t)))) l)
                                        (null? l))
                                    #t
                                    l))))
     (cond
       ((and (equal? pr-labels #t)
             (equal? ex-labels #t))
        (list (list #t)))
       ((equal? pr-labels #t)
        (map true-filter (combine-conjunction-of-dnf ex-labels)))
       ((equal? ex-labels #t)
        (map true-filter (combine-conjunction-of-dnf pr-labels)))
       (else (map true-filter (combine-conjunction-of-dnf (apply append (list pr-labels ex-labels))))))))
 
 ; argument-out-label: argument-graph argument -> dnf
 (define (argument-out-label ag asm arg)
   (let*-values (((ex pr) (partition exception? (argument-premises arg)))
                 ((pr-labels) (let ((l (flatmap (lambda (p) (statement-out-label ag asm (premise-statement p))) pr)))
                                (if (exists (lambda (d) (equal? d (list #t))) l)
                                    #t
                                    l)))
                 ((ex-labels) (let ((l (flatmap (lambda (p) (statement-in-label ag asm (premise-statement p))) ex)))
                                (if (exists (lambda (d) (equal? d (list #t))) l)
                                    #t
                                    l)))
                 ((label) (cond
                            ((or (equal? pr-labels #t)
                                 (equal? ex-labels #t))
                             (list (list #t)))
                            ((and (null? pr-labels)
                                  (null? ex-labels))
                             (list (list #f)))
                            ((null? pr-labels)
                             ex-labels)
                            (else (map true-filter (apply append (list pr-labels ex-labels)))))))
     (display "argument-out(")
     (display (argument-id arg))
     (display "): ")
     (display label)
     (display " - ")
     (display pr-labels)
     (newline)
     label
     ))
 
 ; statement-in-label: argument-graph (list-of statement) statement -> dnf
 (define (statement-in-label ag asm s)
   (let ((standard (proof-standard ag s)))
     (cond 
       ((member s asm) (list (list #t)))
       ((member (statement-complement s) asm) (list (list s)))
       ((eq? standard 'dv) (dv-in-label ag asm s))
       ((eq? standard 'ba) (ba-in-label ag asm s))
       (else (list (list s))))))
 
 ; statement-out-label: argument-graph (list-of statement) statement -> dnf
 (define (statement-out-label ag asm s)
   (let ((standard (proof-standard ag s)))
     (cond 
       ((member s asm) (list (list (statement-complement s))))
       ((member (statement-complement s) asm) (list (list #t)))
       ((eq? standard 'dv) (dv-out-label ag asm s))
       ((eq? standard 'ba) (ba-out-label ag asm s))
       (else (list (list (statement-complement s)))))))
 
 ; dv-in-label: argument-graph (list-of statement) statement -> dnf
 (define (dv-in-label ag asm s)
   (let* ((pro-args (pro-arguments ag s))
          (con-args (con-arguments ag s))
          (pro-labels (let ((l (flatmap (lambda (arg) (argument-in-label ag asm arg)) pro-args)))
                        (if (exists (lambda (c) (equal? c (list #t))) l)
                            #t
                            l)))
          (con-labels (let ((l (map (lambda (arg) (argument-out-label ag asm arg)) con-args)))
                        (if (for-all (lambda (d) (equal? d (list (list #t)))) l)
                            #t
                            (if (exists (lambda (c) (equal? c (list (list #f)))) l)
                                #f
                                l))))
          (label (cond 
                   ((equal? con-labels #f)
                    (list (list s)))
                   ((and (equal? pro-labels #t)
                         (equal? con-labels #t))
                    (list (list #t)))
                   ((equal? pro-labels #t)
                    (cons (list s) (map true-filter (combine-conjunction-of-dnf con-labels))))
                   ((equal? con-labels #t)
                    (cons (list s) pro-labels))                   
                   (else (cons (list s) (map true-filter (combine-conjunction-of-dnf (cons pro-labels con-labels))))))))
     (display "in(")
     (display s)
     (display "): ")
     (display label)
     (newline)
     (display "  pro-labels: ")
     (display pro-labels)
     (newline)
     (display "  con-labels: ")
     (display con-labels)
     (newline)
     label))
 
 (define (dv-out-label ag asm s)
   (let* ((pro-args (pro-arguments ag s))
          (con-args (con-arguments ag s))
          (pro-labels (let ((l (map (lambda (arg) (argument-out-label ag asm arg)) pro-args)))
                        (if (for-all (lambda (d) (equal? d (list (list #t)))) l)
                            #t
                            (if (exists (lambda (c) (equal? c (list (list #f)))) l)
                                #f
                                l))))
          (con-labels (let ((l (flatmap (lambda (arg) (argument-in-label ag asm arg)) con-args)))
                        (if (exists (lambda (d) (equal? d (list #t))) l)
                            #t
                            l)))
          ; handle (null? con-labels) case
          (label (cond                   
                   ((or (equal? pro-labels #t)
                        (equal? con-labels #t))
                    (list (list #t)))
                   ((equal? pro-labels #f)
                    (cons (list (statement-complement s)) con-labels))
                   (else
                    (cons (list (statement-complement s)) (map true-filter (apply append (list con-labels (combine-conjunction-of-dnf pro-labels)))))))))
     (display "out(")
     (display s)
     (display "): ")
     (display label)
     (newline)
     (display "  pro-labels: ")
     (display pro-labels)
     (newline)
     (display "  con-labels: ")
     (display con-labels)
     (newline)
     label))
 
 (define (ba-in-label ag asm s)
   (list (list s)))
 
 (define (ba-out-label ag asm s)
   (list (list (statement-complement s))))
 
 )





