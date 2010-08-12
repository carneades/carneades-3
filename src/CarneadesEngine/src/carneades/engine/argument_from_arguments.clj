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

(ns carneades.engine.argument-from-arguments
  (:use clojure.contrib.pprint
        carneades.engine.utils
        carneades.engine.argument
        [carneades.engine.argument-search :only (response)]))

;; generator for arguments from argument graphs
 
;; Essentially this generator interprets arguments as propositional implications
;; ("rules") 
;; and uses defeasible modus ponens to generate arguments
;; from these implications.


;; type generator: statement state  -> (seq-of response)
(defn generate-arguments-from-argument-graph [ag-as-kb]
  "generate-arguments: argument-graph -> generator"
  (fn [goal state]
    (let [subs (sget state :substitutions)
          proposition (subs goal)
          pro-args (pro-arguments ag-as-kb proposition)]
      (map (fn [arg]
             ;; no new substitutions, since propositional
             (response subs (assoc arg :id (gensym "a"))))
           pro-args))))
