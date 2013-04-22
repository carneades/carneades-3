(ns carneades.engine.triplestore-test
  (:use carneades.engine.triplestore
        clojure.test)
  (:require [carneades.engine.argument-generator :as generator]))

(deftest test-generate-arguments-from-triplestore-grounded
  (let [url "http://dbpedia.org/sparql"
        goal '(dbpedia/Carneades rdf/type dbpedia-owl/Philosopher)
        triplestore-generator (generate-arguments-from-triplestore url)
        responses (generator/generate triplestore-generator goal #{})]
    (is (not (empty? responses)))
    (is (= (count responses) 1))
    (is (= (-> (first responses) :argument :conclusion) goal))))
