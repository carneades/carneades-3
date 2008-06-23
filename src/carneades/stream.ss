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
 (streams)

 (export stream-append stream-interleave stream-accumulate
         stream-flatten stream-flatmap stream->list)
 
 (import
  (rnrs base)
  (only (rnrs r5rs (6)) delay)
  (srfi/41 streams))

 
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