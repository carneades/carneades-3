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

(deftest test-format-statement
  (is (= "(undercut urn:uuid:9149f01d-a538-4729-abfc-a5be3e724f85)"
         (statement-formatted (map->statement '{:atom (undercut urn:uuid:9149f01d-a538-4729-abfc-a5be3e724f85), :header nil, :positive true, :weight nil, :main false, :standard :pe, :text {}, :con #{}, :pro #{urn:uuid:cbfe61db-fc63-4597-918b-8984334ade6b}, :premise-of #{}, :value 1.0, :id urn:uuid:5ac4f0ff-22f1-4fa2-9180-ad63698bd938}) true)
)))