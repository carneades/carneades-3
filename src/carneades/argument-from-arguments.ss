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

(module argument-from-arguments mzscheme
  
  ; generator for arguments from argument graphs
  
  ; Essentially this generator interprets arguments as propositional implications ("rules") 
  ; and uses defeasible modus ponens to generate arguments from these implications.
  
  (require "stream.ss")
  (require "argument.ss")
  (require "statement.ss")
  (require "argument-search.ss")
  
  (provide generate-arguments-from-argument-graph)
  
  ; type generator: statement state  -> (stream-of response)
 
  ; generate-arguments-from-argument-graph: argument-graph -> generator
  (define (generate-arguments-from-argument-graph ag1)
    (lambda (goal state)
      (let ((ag2 (state-arguments state))
            (subs (state-substitutions state)))
        (let* ((p (subs goal))
               (node (get-node ag1 (statement-atom p))))
          (stream-flatmap 
           (lambda (arg-id)
             (let ((arg (get-argument ag1 arg-id)))
               (if arg
                   (let ((arg  (make-argument (gensym 'a) ; new id required to assure uniqueness
                                              (argument-direction arg)
                                              (argument-conclusion arg)
                                              (argument-premises arg)
                                              (argument-scheme arg))))
                     (stream (make-response subs arg))) ; no new substitutions, since propositional
                   ; else fail, no argument with the given id
                   (stream))))
           (apply stream (if (not node)
                             null
                             (if (statement-negative? p)
                                 (node-con node)
                                 (node-pro node)))))))))
  
  ) ; end of module