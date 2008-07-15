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
 (carneades stream)

 (export stream-interleave stream-accumulate stream-flatten stream-flatmap
         
         ; from srfi 41
         define-stream stream stream-null stream-null? stream-cons stream? stream-pair? stream-car
         stream-cdr stream-lambda port->stream stream stream->list list->stream
         stream-append stream-concat stream-drop stream-drop-while
         stream-filter stream-fold stream-for-each stream-from
         stream-iterate stream-length stream-let stream-map stream-match
         stream-of stream-range stream-ref stream-reverse stream-scan
         stream-take stream-take-while stream-unfold stream-unfolds
         stream-zip)
         
 
 (import
  (rnrs base)
  (carneades lib srfi streams))

 
 (define (stream-interleave s1 s2)
   (if (stream-null? s1)
       s2
       (stream-cons (stream-car s1)
                    (stream-interleave s2 (stream-cdr s1)))))
 
 (define (stream-accumulate combiner initial-value stream)
   (if (stream-null? stream)
       initial-value
       (combiner (stream-car stream)
                 (stream-accumulate combiner
                                    initial-value
                                    (stream-cdr stream)))))
 
 (define (stream-flatten stream)
   (stream-accumulate stream-interleave stream-null stream))
 
 (define (stream-flatmap f s) (stream-flatten (stream-map f s)))

 
 ) ; end of stream library