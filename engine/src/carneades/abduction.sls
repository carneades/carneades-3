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
 (carneades abduction)
 
 (export statement-in-label statement-out-label
         argument-in-label argument-out-label)
 
 (import (rnrs)
         (carneades argument)
         (carneades statement))
 
 
 (define (flatmap f l) (apply append (map f l))) 
 
 (define verum-clause (list #t))
 (define verum (list verum-clause))
 (define falsum-clause (list #f))
 (define falsum (list falsum-clause))
   
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
                                (if (or (for-all (lambda (l) (equal? l verum)) l)
                                        (null? l))
                                    #t
                                    l)))
                 ((ex-labels) (let ((l (map (lambda (p) (statement-out-label ag asm (premise-statement p))) ex)))
                                (if (or (for-all (lambda (l) (equal? l verum)) l)
                                        (null? l))
                                    #t
                                    l)))
                 ((label) (cond
                            ((and (equal? pr-labels #t)
                                  (equal? ex-labels #t))
                             verum)
                            ((equal? pr-labels #t)
                             (map true-filter (combine-conjunction-of-dnf ex-labels)))
                            ((equal? ex-labels #t)
                             (map true-filter (combine-conjunction-of-dnf pr-labels)))
                            (else (map true-filter (combine-conjunction-of-dnf (apply append (list pr-labels ex-labels))))))))
;     (display "argument-in(")
;     (display (argument-id arg))
;     (display "): ")
;     (display label)
;     (newline)
;     (display " - pr: ")
;     (display pr-labels)
;     (newline)
;     (display " - ex: ")
;     (display ex-labels)
;     (newline)
     label))
 
 ; argument-out-label: argument-graph argument -> dnf
 (define (argument-out-label ag asm arg)
   (let*-values (((ex pr) (partition exception? (argument-premises arg)))
                 ((pr-labels) (let ((l (flatmap (lambda (p) (statement-out-label ag asm (premise-statement p))) pr)))
                                (if (exists (lambda (d) (equal? d verum-clause)) l)
                                    #t
                                    l)))
                 ((ex-labels) (let ((l (flatmap (lambda (p) (statement-in-label ag asm (premise-statement p))) ex)))
                                (if (exists (lambda (d) (equal? d verum-clause)) l)
                                    #t
                                    l)))
                 ((label) (cond
                            ((or (equal? pr-labels #t)
                                 (equal? ex-labels #t))
                             verum)
                            ((and (null? pr-labels)
                                  (null? ex-labels))
                             falsum)
                            ((null? pr-labels)
                             ex-labels)
                            (else (map true-filter (apply append (list pr-labels ex-labels)))))))
;     (display "argument-out(")
;     (display (argument-id arg))
;     (display "): ")
;     (display label)
;     (newline)
;     (display " - pr: ")
;     (display pr-labels)
;     (newline)
;     (display " - ex: ")
;     (display ex-labels)
;     (newline)
     label
     ))
 
 ; statement-in-label: argument-graph (list-of statement) statement -> dnf
 (define (statement-in-label ag asm s)
   (let ((standard (proof-standard ag s)))
     (cond 
       ((member s asm) verum)
       ((member (statement-complement s) asm) (list (list s)))
       ((eq? standard 'dv) (dv-in-label ag asm s))
       ((or (eq? standard 'ba)
            (eq? standard 'pe))
        (ba-in-label ag asm s))
       (else (list (list s))))))
 
 ; statement-out-label: argument-graph (list-of statement) statement -> dnf
 (define (statement-out-label ag asm s)
   (let ((standard (proof-standard ag s)))
     (cond 
       ((member s asm) (list (list (statement-complement s))))
       ((member (statement-complement s) asm) verum)
       ((eq? standard 'dv) (dv-out-label ag asm s))
       ((or (eq? standard 'ba)
            (eq? standard 'pe))
        (ba-out-label ag asm s))
       (else (list (list (statement-complement s)))))))
 
 ; dv-in-label: argument-graph (list-of statement) statement -> dnf
 (define (dv-in-label ag asm s)
   (let* ((pro-args (pro-arguments ag s))
          (con-args (con-arguments ag s))
          (pro-labels (let ((l (flatmap (lambda (arg) (argument-in-label ag asm arg)) pro-args)))
                        (if (exists (lambda (c) (equal? c verum-clause)) l)
                            #t
                            l)))
          (con-labels (let ((l (map (lambda (arg) (argument-out-label ag asm arg)) con-args)))
                        (if (for-all (lambda (d) (equal? d verum)) l)
                            #t
                            (if (exists (lambda (c) (equal? c falsum)) l)
                                #f
                                l))))
          (label (cond 
                   ((equal? con-labels #f)
                    (list (list s)))
                   ((and (equal? pro-labels #t)
                         (equal? con-labels #t))
                    verum)
                   ((equal? pro-labels #t)
                    (cons (list s) (map true-filter (combine-conjunction-of-dnf con-labels))))
                   ((equal? con-labels #t)
                    (cons (list s) pro-labels))                   
                   (else (cons (list s) (map true-filter (combine-conjunction-of-dnf (cons pro-labels con-labels))))))))
     ;     (display "dv-in(")
     ;     (display s)
     ;     (display "): ")
     ;     (display label)
     ;     (newline)
     ;     (display "  pro-labels: ")
     ;     (display pro-labels)
     ;     (newline)
     ;     (display "  con-labels: ")
     ;     (display con-labels)
     ;     (newline)
     label))
 
 ; dv-out-label: argument-graph (list-of statement) statement -> dnf
 (define (dv-out-label ag asm s)
   (let* ((pro-args (pro-arguments ag s))
          (con-args (con-arguments ag s))
          (pro-labels (let ((l (map (lambda (arg) (argument-out-label ag asm arg)) pro-args)))
                        (if (for-all (lambda (d) (equal? d verum)) l)
                            #t
                            (if (exists (lambda (c) (equal? c falsum)) l)
                                #f
                                l))))
          (con-labels (let ((l (flatmap (lambda (arg) (argument-in-label ag asm arg)) con-args)))
                        (if (exists (lambda (d) (equal? d verum-clause)) l)
                            #t
                            l)))
          ; handle (null? con-labels) case
          (label (cond                   
                   ((or (equal? pro-labels #t)
                        (equal? con-labels #t))
                    verum)
                   ((equal? pro-labels #f)
                    (cons (list (statement-complement s)) con-labels))
                   (else
                    (cons (list (statement-complement s)) (map true-filter (apply append (list con-labels (combine-conjunction-of-dnf pro-labels)))))))))
     ;     (display "dv-out(")
     ;     (display s)
     ;     (display "): ")
     ;     (display label)
     ;     (newline)
     ;     (display "  pro-labels: ")
     ;     (display pro-labels)
     ;     (newline)
     ;     (display "  con-labels: ")
     ;     (display con-labels)
     ;     (newline)
     label))
 
 
 ; ba-in-label: argument-graph (list-of statement) statement -> dnf
 (define (ba-in-label ag asm s)
   (let* ((pro-args (pro-arguments ag s))
          (con-args (con-arguments ag s))
          (tmp-label (remp (lambda (c) (equal? c falsum-clause))
                       (flatmap (lambda (pro-arg)
                                  (let* ((w (argument-weight pro-arg))
                                         (greater-cons (filter (lambda (con-arg) (<= w (argument-weight con-arg))) con-args))
                                         (pro-label (argument-in-label ag asm pro-arg))
                                         (con-labels (let ((l (map (lambda (a) (argument-out-label ag asm a)) greater-cons)))
                                                       (if (exists (lambda (d) (equal? d falsum)) l)
                                                           #f
                                                           l)))
                                         (iter-label (if (not con-labels)
                                                         falsum
                                                         (combine-conjunction-of-dnf
                                                          (cons 
                                                           (if (equal? pro-label verum)
                                                               '()
                                                               pro-label)
                                                           (remp 
                                                            (lambda (d) (equal? d verum))
                                                            con-labels))))))
;                                    (display "iter-in-label(")
;                                    (display (argument-id pro-arg))
;                                    (display "): ")
;                                    (display iter-label)
;                                    (newline)
                                    (if (null? iter-label)
                                        verum-clause
                                        iter-label)))
                                pro-args)))
          (label (if (exists (lambda (c) (equal? c #t)) tmp-label)
                     verum
                     (cons (list s) tmp-label))))
;     (display "ba-in(")
;     (display s)
;     (display "): ")
;     (display label)
;     (newline)
     label))
     
 
 ; ba-out-label: argument-graph (list-of statement) statement -> dnf
 (define (ba-out-label ag asm s)
   (let* ((pro-args (pro-arguments ag s))
          (con-args (con-arguments ag s))
          (label-conjunction
           (remp (lambda (c) (equal? c #t))
                 (map (lambda (pro-arg)
                        (let* ((w (argument-weight pro-arg))
                               (greater-cons (filter (lambda (con-arg) (<= w (argument-weight con-arg))) con-args))
                               (pro-label (argument-out-label ag asm pro-arg))
                               (con-labels (let ((l (flatmap (lambda (a) (argument-in-label ag asm a)) greater-cons)))
                                             (cond
                                               ((null? l)
                                                #f)
                                               ((exists (lambda (c) (equal? c verum-clause)) l)
                                                #t)
                                               (else l))))
                               (iter-label (cond
                                             ((or (equal? pro-label verum)
                                                  (equal? con-labels #t))
                                              #t)
                                             ((and (equal? pro-label falsum)
                                                   (equal? con-labels #f))
                                              #f)
                                             ((equal? pro-label falsum)
                                              con-labels)
                                             ((equal? con-labels #f)
                                              pro-label)
                                             (else (apply append (list pro-label con-labels))))))
;                          (display "iter-out-label(")
;                          (display (argument-id pro-arg))
;                          (display "): ")
;                          (display iter-label)
;                          (newline)
                          iter-label))
                      pro-args)))
          (label (cond
                   ((null? label-conjunction) verum)
                   ((exists (lambda (c) (equal? c #f)) label-conjunction)
                    (list (list (statement-complement s))))
                   (else (cons (list (statement-complement s)) (combine-conjunction-of-dnf label-conjunction))))))
;     (display "ba-out(")
;     (display s)
;     (display "): ")
;     (display label)
;     (newline)
     label))
 
 )





