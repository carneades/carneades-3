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
 (carneades argument-from-arguments)
 ; generator for arguments from argument graphs
 
 ; Essentially this generator interprets arguments as propositional implications ("rules") 
 ; and uses defeasible modus ponens to generate arguments from these implications.
 
 (export generate-arguments-from-argument-graph)
 
 (import (rnrs base)
         (carneades statement)
         (only (carneades system) gensym)
         (carneades stream)
         (carneades argument)
         (carneades argument-search))
 
 ; type generator: statement state  -> (stream-of response)
 
 (define (generate-arguments-from-argument-graph ag1)
   (lambda (goal state)
     (let ((ag2 (state-arguments state))
           (subs (state-substitutions state)))
       (let* ((p (apply-substitution subs goal))
              (pro-args (pro-arguments ag1 p)))
         (stream-map 
          (lambda (arg1)
            (let ((arg2 (make-argument (gensym 'a) ; new id required to assure uniqueness
                                       (argument-direction arg1)
                                       (argument-conclusion arg1)
                                       (argument-premises arg1)
                                       (argument-scheme arg1))))
              (make-response subs arg2))) ; no new substitutions, since propositional
          (list->stream pro-args))))))
 ) ; end of module