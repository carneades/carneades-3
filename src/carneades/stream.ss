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

;(module stream mzscheme
;  ; The SRFI 40 implementation of streams extended with 
;  ; other stream functions from Abelson and Sussman's 
;  ; "Structure and Interpretation of Computer Programs"
;  
;  (require (lib "stream.ss" "srfi" "40"))
;  (provide (all-from (lib "stream.ss" "srfi" "40")))
;  (provide (all-defined))
;  
;  ; stream-append: stream stream -> stream
;  (define (stream-append s1 s2)
;     (if (stream-null? s1) 
;         s2
;         (stream-cons (stream-car s1)
;                      (stream-append (stream-cdr s1) s2))))
;  
;  (define (stream-interleave s1 s2)
;    (if (stream-null? s1)
;        s2
;        (stream-cons (stream-car s1)
;                     (stream-delay (stream-interleave s2 (stream-delay (stream-cdr s1)))))))
;  
;  (define (stream-accumulate combiner initial-value stream)
;    (if (stream-null? stream)
;        initial-value
;        (combiner (stream-car stream)
;                  (stream-delay (stream-accumulate combiner
;                                                   initial-value
;                                                   (stream-delay (stream-cdr stream)))))))
;  
;  (define (stream-flatten stream)
;    (stream-accumulate stream-interleave stream-null stream))
;  
;  (define (stream-flatmap f s) (stream-flatten (stream-map f s)))
;  
;  ; stream->list: (stream-of x) -> (list-of x)
;  ; The value of (stream->list s) is undefined is s is infinite
;  (define (stream->list s)
;    (if (stream-null? s) 
;        null
;        (cons (stream-car s) (stream->list (stream-cdr s)))))
;  
;  ) ; end of stream module

#!r6rs

(library 
 (streams)

 (export stream-append stream-interleave stream-accumulate
         stream-flatten stream-flatmap stream->list)
 
 (import
  (rnrs base)
  (only (rnrs r5rs (6)) delay)
  (carneades srfi streams)) ; SRFI 41
 
 (define (stream-interleave s1 s2)
   (if (stream-null? s1)
       s2
       (stream-cons (stream-car s1)
                    (delay (stream-interleave s2 (delay (stream-cdr s1)))))))
 
 (define (stream-accumulate combiner initial-value stream)
   (if (stream-null? stream)
       initial-value
       (combiner (stream-car stream)
                 (delay (stream-accumulate combiner
                                                  initial-value
                                                  (delay (stream-cdr stream)))))))
 
 (define (stream-flatten stream)
   (stream-accumulate stream-interleave stream-null stream))
 
 (define (stream-flatmap f s) (stream-flatten (stream-map f s)))

 
 ) ; end of stream library