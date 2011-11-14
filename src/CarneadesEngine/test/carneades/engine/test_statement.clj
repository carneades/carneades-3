;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.test-statement
  (:use clojure.test
        carneades.engine.statement))

(deftest test-variable?
  (is (variable? '?p))
  (is (variable? '?r1))
  (is (not (variable? 'r2)))
  (is (not (variable? "'?r1"))))

(deftest test-constant?
  (is (constant? 42))
  (is (constant? "plato"))
  (is (constant? (symbol "xyz")))
  (is (not (constant? '?r1))))

(deftest test-statement-complement?
  (is (= '(not (mother Tom Garcia))
         (literal-complement '(mother Tom Garcia))))
  (is (= '(mother Tom Garcia)
         (literal-complement '(not (mother Tom Garcia))))))

(deftest test-statement-atom
  (is (= '(x ?r1 ?r2) (:atom (make-statement :atom '(x ?r1 ?r2)))))
  (is (= '(not (x ?r1 ?r2)) (:atom (make-statement :atom '(not (x ?r1 ?r2)))))))

