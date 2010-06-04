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

(ns carneades.engine.test-abduction
  (:use clojure.test
        clojure.contrib.pprint
        clojure.contrib.trace
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.abduction))

(deftest test-in-label
  (let [u "u"
        v "v"
        t "t"
        w "w"
        r "r"
        s "s"
        q "q"
        p "p"
        a4 (make-arg :a4 (pro r
                              (pm u)
                              (ex v)))
        a3 (argument :a3 false 0.6 :con r [(pm t)] nil)
        a5 (make-arg :a5 (con r
                              (pm w)))
        a1 (argument :a1 false 0.6 :con p [(pm r)] nil)
        a2 (argument :a2 false 0.4 :pro p [(pm s) (pm q)] nil)
        ag (assoc-standard (assert-arguments
                            (reject
                             (accept *empty-argument-graph* [s w])
                             [q v t])
                            [a1 a2 a3 a4 a5])
                           :ba
                           [u v t w r s q p])
        asm #{(statement-complement q)
              (statement-complement v)
              (statement-complement t)
              w s}
        in-label-p (statement-in-label ag asm p)]
    ;; (dotrace [combine-conjunction-of-dnf
    ;;           statement-in-label
    ;;           ba-in-label]
    ;;          (statement-in-label ag asm p))
    (is (= (count in-label-p) 2))
    (is (or (and (= (first in-label-p) (list p))
                 (= (second in-label-p) (list q)))
            (and (= (first in-label-p) (list q))
                 (= (second in-label-p) (list p)))))
    ;; (let [x (statement-in-label ag asm p)]
    ;;   (printf "=============\n")
    ;;   (pprint x)
    ;;   (printf "=============\n"))
    ))

