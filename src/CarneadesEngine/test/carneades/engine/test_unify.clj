;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.test-statement
  (:use clojure.test
        carneades.engine.statement
        carneades.engine.unify))

(deftest test-unify
  (is (= {'?x '?y} (unify '?x '?y)))
  (is (= {'?x :a} (unify '?x :a)))
  (is (= nil (unify :a :b)))
  (is (= {} (unify :a :a)))
  (is (= {} (unify 1 1)))
  (is (= nil (unify 1 2)))
  (is (= {} (unify "this" "this")))
  (is (= nil (unify "this" "that")))
  (is (= {} (unify 'a 'a)))
  (is (= nil (unify 'a 'b)))
  (is (= {} (unify true true)))
  (is (= nil (unify true false)))
  (is (= {'?x 3 '?y 2} (unify '(1 2 ?x 4) '(1 ?y 3 4))))
  (is (= {'?x 3 '?y 2} (unify [1 2 '?x 4] [1 '?y 3 4])))
  (is (= {'?x 1 '?y 2} (unify {:a '?x :b 2} {:a 1 :b '?y})))
  (is (= {'?x a}) (unify (make-statement :atom '(p a b))
                         (make-statement :atom '(p ?x b))))
  (is (= {'?x a}) (unify (make-statement :atom '(p a b))
                         '(p ?x b)))
  (is (= nil (unify (make-statement) (make-statement))))
  (is (= nil (unify (make-statement :id 's1 :positive true)
                    (make-statement :id 's2 :positive false))))
  (is (= {'?x 'b} (unify (make-statement :positive false :atom '(p a b c))
                    '(not (p a ?x c))))))

  ; to do: further test, e.g. for compound terms and statements used as terms
  
  
         
         