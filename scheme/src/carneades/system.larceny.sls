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
 
 (export system* system pretty-print tcp-connect 
 	     (rename (gensym1 gensym)))
 
 (import (rnrs)
         (primitives gensym system pretty-print))
         
  
 (define (system* . args) 
   (raise-continuable (make-message-condition "system* procedure not provided by Larceny.")))
 
 ; tcp-connect: string integer -> input-port
 (define (tcp-connect hostname port)
 	(raise-continuable (make-message-condition "tcp-connect procedure not provided by Larceny.")))
 
 (define gensym1
   (case-lambda 
     (() 
      (gensym "g"))
     ((prefix) ; symbol or string
      (gensym prefix))))
 )