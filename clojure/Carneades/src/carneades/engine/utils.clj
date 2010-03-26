;;; Carneades Argumentation Library and Tools.
;;; Copyright (C) 2010 Thomas F. Gordon, Fraunhofer FOKUS, Berlin
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

(ns carneades.engine.utils)

(defn boolean? [x]
  (instance? Boolean x))

(defn nonemptyseq? [x]
  "Returns true if x is a nonempty sequence"
  (and (seq? x) (not (empty? x))))

(defn conj-ifnot [pred col x]
  "conjoin col and x if the binary predicate pred is false
   for all values in col "
  (if (some #(pred x %) col)
    col
    (conj col x)))

;; Example:
;;(conj-ifnot premise= (list (pm '(father T D))
;; (pm '(mother Pierre XYZ)))
;; (pm (struct fatom "%s -> %s" '(father T D))))
;; returns:
;; ({:atom (father T D), :polarity true, :role nil, :type
;; :carneades.engine.argument/ordinary-premise}
;; {:atom (mother Pierre XYZ), :polarity true, :role nil,
;; :type :carneades.engine.argument/ordinary-premise})

(defn union-if
  "Returns the union of s1 and s2, (pred x y) is true for each
   (x y) couple in the union"
  ([pred] {})
  ([pred s1] s1)
  ([pred s1 s2]
     (if (< (count s1) (count s2))
       (reduce #(conj-ifnot pred %1 %2) s1 s2)
       (reduce #(conj-ifnot pred %1 %2) s2 s1))))

;; (union-if premise= (list (pm '(father T D))
;; (pm '(mother Pierre XYZ)))
;; (list (pm (struct fatom "x y z" '(father T D) ))))
