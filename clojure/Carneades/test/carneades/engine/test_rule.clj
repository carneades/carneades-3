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

(ns carneades.engine.test-rule
  (:use clojure.test
        carneades.engine.rule))

(deftest test-condition-statement
  (is (= '(x y z) (condition-statement '(assuming (x y z)))))
  (is (= '(x y z) (condition-statement '(unless (x y z)))))
  (is (= '(x y z) (condition-statement '(x y z)))))

(deftest test-predicate
  (is (= 'mother (predicate '(unless (mother Tom Garcia))))))

(deftest test-rule
  (is (= {:id 'r1,
          :strict false,
          :head '((goods ?c)),
          :body '(((movable ?c) (unless (money ?c))))}
         (rule r1 (if (and (movable ?c)
                           (unless (money ?c)))
                    (goods ?c)))))
  (is (= {:id 'r3, :strict false, :head '((money ?x)), :body '(((coins ?x)))}
         (rule r3 (if (coins ?x) (money ?x)))))
  (is (= {:id 'r4,
          :strict false,
          :head '((light ?x) (shipable ?x)),
          :body '(((movable ?x)))}
         (rule r4
               (if (movable ?x)
                 (and (light ?x)
                      (shipable ?x))))))
  (is (= {:id 'r5,
          :strict false,
          :head '((convenient ?x)),
          :body '(((light ?x) (shipable ?x)))}
         (rule r5
               (if (and (light ?x)
                        (shipable ?x))
                 (convenient ?x)))))
  (is (= {:id 'r6,
          :strict false,
          :head '((not (goods ?x))),
          :body '(((edible ?x)))}
         (rule r6 
               (if (edible ?x) 
                 (not (goods ?x)))))))
