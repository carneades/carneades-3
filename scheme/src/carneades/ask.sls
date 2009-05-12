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
 (carneades ask)
 
 ; Provides a way to ask users questions. Designed for use with dialogue managers and form servers.
 
 (export ask)

 (import (rnrs)
         (carneades base)
         (carneades stream)
         (carneades argument-search))
   
 ; type generator: statement state -> (stream-of response)
 
 ; ask: (statement -> boolean) -> generator
 (define (ask askable?)
   (lambda (goal state) 
     (if (askable? goal)
         (raise `(ask ,((state-substitutions state) goal), state))
         (stream)))) ; empty stream
             
 
 ) ;end of module
