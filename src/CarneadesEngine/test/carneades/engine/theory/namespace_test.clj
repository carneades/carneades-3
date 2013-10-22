(ns carneades.engine.theory.namespace-test
  (:require [carneades.engine.theory.namespace :as tn]
            [midje.sweet :refer :all]))

(fact "to-literal absolute works."
      (let [sexp1 '(a b c)
            sexp2 '(owl:a (owl:b rdfs:c d))]
        (expect (tn/to-absolute-literal sexp1 {}) => sexp1)
        (expect (tn/to-absolute-literal sexp1 {"" "http://www.markosproject.eu/ontologies/copyright#"})
                => '(http://www.markosproject.eu/ontologies/copyright#a
                     http://www.markosproject.eu/ontologies/copyright#b
                     http://www.markosproject.eu/ontologies/copyright#c))
        (expect (tn/to-absolute-literal sexp2 {"" "http://www.markosproject.eu/ontologies/copyright#"
                                               "owl" "http://www.w3.org/2002/07/owl#",
                                               "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
                                               })
                => '(http://www.w3.org/2002/07/owl#a
                     (http://www.w3.org/2002/07/owl#b
                      http://www.w3.org/2000/01/rdf-schema#c
                      http://www.markosproject.eu/ontologies/copyright#d)))
        (expect (tn/to-absolute-literal sexp2 {"owl" "http://www.w3.org/2002/07/owl#",
                                               "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
                                               })
                => '(http://www.w3.org/2002/07/owl#a
                     (http://www.w3.org/2002/07/owl#b
                      http://www.w3.org/2000/01/rdf-schema#c
                      d)))
        (expect (tn/to-absolute-literal '(http://www.w3.org/2002/07/owl#a
                                          (http://www.w3.org/2002/07/owl#b
                                           http://www.w3.org/2000/01/rdf-schema#c))
                                        {"owl" "http://www.w3.org/2002/07/owl#",
                                         "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
                                         "" "http://something"
                                         })
                => '(http://www.w3.org/2002/07/owl#a
                     (http://www.w3.org/2002/07/owl#b
                      http://www.w3.org/2000/01/rdf-schema#c)))))
