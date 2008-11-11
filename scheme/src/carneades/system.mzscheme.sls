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
 (carneades system)
 
 (export system pretty-print tcp-connect gensym null printf)
 
 (import (rnrs)
         (only (scheme system) system)
         (only (scheme pretty) pretty-print)
         (only (scheme tcp) tcp-connect)
         (carneades lib srfi format) 
         ;(only (scheme base) gensym)
         )
 
 (define null '())
 
 (define (printf format-string . args)
   (apply format `(,(current-output-port)  ,format-string ,@args))
   (flush-output-port (current-output-port)))
    
 (define gensym-counter 0)
 
 (define gensym
   (case-lambda 
     (() 
      (set! gensym-counter (+ 1 gensym-counter))
      (string->symbol (string-append "g" 
                                     (number->string gensym-counter))))
     ((prefix) ; symbol or string
      (set! gensym-counter (+ 1 gensym-counter))
      (string->symbol (string-append 
                       (if (symbol? prefix) 
                           (symbol->string prefix) 
                           prefix)
                       (number->string gensym-counter))))))
 
 )