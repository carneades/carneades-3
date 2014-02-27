;; Copyright (c) 2010 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.engine.test-unify
  (:require [carneades.engine.statement :refer :all]
            [carneades.engine.unify :refer :all]
            [midje.sweet :refer :all] ))

(fact "The unify function works."
      (expect (unify '?x '?y) => {'?x '?y})
      (expect (unify '?x :a) => {'?x :a})
      (expect (unify :a :b) => nil)
      (expect (unify :a :a) => {})
      (expect (unify 1 1) => {})
      (expect (unify 1 2) => nil)
      (expect (unify "this" "this") => {})
      (expect (unify "this" "that") => nil)
      (expect (unify 'a 'a) => {})
      (expect (unify 'a 'b) => nil)
      (expect (unify true true) => {})
      (expect (unify true false) => nil)
      (expect (unify '(1 2 ?x 4) '(1 ?y 3 4)) => {'?x 3 '?y 2})
      (expect (unify [1 2 '?x 4] [1 '?y 3 4]) => {'?x 3 '?y 2})
      (expect (unify {:a '?x :b 2} {:a 1 :b '?y}) => {'?x 1 '?y 2})
      (expect (unify (make-statement :atom '(p a b))
                         (make-statement :atom '(p ?x b))) => {'?x 'a})
      (expect (unify (make-statement :atom '(p a b))
                         '(p ?x b)) => {'?x 'a})
      (expect (unify (make-statement) (make-statement)) => nil)
      (expect (unify (make-statement :id 's1 :positive true)
                          (make-statement :id 's2 :positive false)) => nil)
      (expect (unify (make-statement :positive false :atom '(p a b c))
                               '(not (p a ?x c))) => {'?x 'b}))

(fact "The renaming of the variables works."
  (let [s '(a ?x ?y)
        slet '(let [x (* ?I 0.04) min 6000] (if (< x min) min x))]
    (expect  (second (rename-variables {} s)) =not=> s)
    (expect (second (rename-variables {} slet)) =not=> slet)))
