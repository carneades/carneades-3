(ns carneades.engine.triplestore-test
  (:use carneades.engine.triplestore
        clojure.test
        clojure.pprint)
  (:require [carneades.engine.argument-generator :as generator]))

(deftest test-generate-arguments-from-triplestore-grounded
  (let [url "http://dbpedia.org/sparql"
        goal '(dbpedia/Carneades rdf/type dbpedia-owl/Philosopher)
        subs '{?a "a" ?b "b"}
        triplestore-generator (generate-arguments-from-triplestore url)
        responses (generator/generate triplestore-generator goal subs)]
    (is (not (empty? responses)))
    (is (= (count responses) 1))
    (is (= (-> (first responses) :argument :conclusion) goal))
    (is (= (-> (first responses) :substitutions) subs))))

(deftest test-generate-arguments-from-triplestore-notgrounded
  (let [url "http://dbpedia.org/sparql"
        goal '(dbpedia/Carneades rdf/type ?x)
        subs '{?a "a" ?b "b"}
        triplestore-generator (generate-arguments-from-triplestore url)
        responses (generator/generate triplestore-generator goal subs)]
    (pprint responses)
    (is (not (empty? responses)))
    (is (>= (count responses) 10))
    (is (not= (:substitutions (first responses)) subs))))


