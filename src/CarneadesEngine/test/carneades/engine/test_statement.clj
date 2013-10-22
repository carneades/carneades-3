;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.test-statement
  (:require [carneades.engine.statement :refer :all]
            [midje.sweet :refer :all]))

(fact "The variable? predicates works."
       (expect (variable? '?p) => true)
       (expect (variable? '?r1) => true)
       (expect (not (variable? 'r2)) => true)
       (expect (not (variable? "'?r1")) => true))

(fact "The constant? predicates works."
      (expect (constant? 42) => true)
      (expect (constant? "plato") => true)
      (expect (constant? (symbol "xyz")) => true)
      (expect (not (constant? '?r1)) => true))

(fact "The literal-complement function works."
      (expect  (literal-complement '(mother Tom Garcia)) => '(not (mother Tom Garcia)))
      (expect (literal-complement '(not (mother Tom Garcia))) => '(mother Tom Garcia)))

(fact "The literal-atom function works."
      (expect (literal-atom (make-statement :atom '(x ?r1 ?r2))) => '(x ?r1 ?r2))
      (expect (literal-atom (make-statement :atom '(not (x ?r1 ?r2)))) => '(x ?r1 ?r2)))

(fact "Converting a literal to a string works."
      (expect (literal->str (map->statement '{:atom (undercut urn:uuid:9149f01d-a538-4729-abfc-a5be3e724f85), :header nil, :positive true, :weight nil, :main false, :standard :pe, :text {}, :con #{}, :pro #{urn:uuid:cbfe61db-fc63-4597-918b-8984334ade6b}, :premise-of #{}, :value 1.0, :id urn:uuid:5ac4f0ff-22f1-4fa2-9180-ad63698bd938})) => "(undercut urn:uuid:9149f01d-a538-4729-abfc-a5be3e724f85)"))
