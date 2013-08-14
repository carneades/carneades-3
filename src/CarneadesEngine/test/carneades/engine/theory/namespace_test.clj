(ns carneades.engine.theory.namespace-test
  (:require [clojure.test :refer :all]
            [carneades.engine.theory.namespace :as tn]))

(deftest test-to-literal-absolute
  (let [sexp1 '(a b c)
        sexp2 '(owl:a (owl:b rdfs:c d))]
    (is (= sexp1 (tn/to-absolute-literal sexp1 {})))
    (is (= '(http://www.markosproject.eu/ontologies/copyright#a
             http://www.markosproject.eu/ontologies/copyright#b
             http://www.markosproject.eu/ontologies/copyright#c)
           (tn/to-absolute-literal sexp1 {"" "http://www.markosproject.eu/ontologies/copyright#"})))
    (is (= '(http://www.w3.org/2002/07/owl#a
             (http://www.w3.org/2002/07/owl#b
              http://www.w3.org/2000/01/rdf-schema#c
              http://www.markosproject.eu/ontologies/copyright#d))
           (tn/to-absolute-literal sexp2 {"" "http://www.markosproject.eu/ontologies/copyright#"
                                          "owl" "http://www.w3.org/2002/07/owl#",
                                          "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
                                          })))
    (is (= '(http://www.w3.org/2002/07/owl#a
             (http://www.w3.org/2002/07/owl#b
              http://www.w3.org/2000/01/rdf-schema#c
              d))
           (tn/to-absolute-literal sexp2 {"owl" "http://www.w3.org/2002/07/owl#",
                                          "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
                                          })))))
