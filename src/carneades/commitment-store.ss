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

(module commitment-store mzscheme
  (require "table.scm")
  (require (lib "list.ss" "srfi" "1"))
  
  (provide empty-commitment-store add-commitment remove-commitment 
           commitments committed?)
  
  ; make-commitment-store: table -> commitment-store
  ; The table maps a party to his set of commitments, represented
  ; as a list of atoms.  
  (define-struct commitment-store (table))
  
  ; empty-commitment-store : void -> commitment-store
  (define (empty-commitment-store) 
    (make-commitment-store (empty-table)))
  
  ; add-commitment: commitment-store party atom  -> commitment-store
  ; Atoms are compared using "eq?".  Should this be changed to "equal?" orsome kind
  ; matching procedure?
  (define (add-commitment cs p a) 
    (let* ((table (commitment-store-table cs))
           (prior-commitments (table-get table p))
           (new-commitments (if (void? prior-commitments) 
                                (list a)
                                (lset-adjoin eq? prior-commitments a)))) 
      (make-commitment-store (table-put table p new-commitments))))
  
  ; remove-commitment: commitment-store party atom -> commitment-store
  ; Here too atoms are compared with "eq?".
  (define (remove-commitment cs p a)
    (let* ((table (commitment-store-table cs))
           (prior-commitments (table-get table p)))
      (if (void? prior-commitments) 
          cs ; no changes
          (make-commitment-store (table-put table p 
                                            (remove (lambda (e) (eq? e a)) 
                                                    prior-commitments))))))
  
  ; committed? : commitment-store party atom -> boolean
  ; This implementation assumes to atoms are equal iff they are identical (eq?).
  (define (committed? cs p a)
    (if (memq a (table-get (commitment-store-table cs) p)) #t #f))
  
  ; commitments : commitment-store party -> atom-list
  (define (commitments cs p)
    (let ((c (table-get (commitment-store-table cs) p)))
      (if (void? c) null c)))
  
) ; commitment-store