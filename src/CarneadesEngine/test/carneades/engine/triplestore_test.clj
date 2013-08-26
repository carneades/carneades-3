(ns carneades.engine.triplestore-test
  (:use carneades.engine.triplestore
        clojure.test
        clojure.pprint)
  (:require [carneades.engine.argument-generator :as generator]
            [carneades.engine.shell :as shell]
            [carneades.engine.caes :as caes]
            [carneades.engine.argument-graph :as ag]
            [carneades.maps.lacij :as lacij]))

(deftest test-generate-arguments-from-triplestore-grounded
  (let [url "http://dbpedia.org/sparql"
        goal '(rdf/type dbpedia/Carneades dbpedia-owl/Philosopher)
        subs '{?a "a" ?b "b"}
        triplestore-generator (generate-arguments-from-triplestore url)
        responses (generator/generate triplestore-generator goal subs)]
    (is (not (empty? responses)))
    (is (= (count responses) 1))
    (is (= (-> (first responses) :argument :conclusion) goal))
    (is (= (-> (first responses) :substitutions) subs))))

(deftest test-generate-arguments-from-triplestore-with-iri
  (let [url "http://dbpedia.org/sparql"
        goal '(rdf/type
               http://dbpedia.org/resource/Carneades
               http://dbpedia.org/ontology/Philosopher)
        subs '{?a "a" ?b "b"}
        triplestore-generator (generate-arguments-from-triplestore url)
        responses (generator/generate triplestore-generator goal subs)]
    (is (not (empty? responses)))
    (is (= (count responses) 1))
    (is (= (-> (first responses) :argument :conclusion) goal))
    (is (= (-> (first responses) :substitutions) subs))))

(deftest test-generate-arguments-from-triplestore-notgrounded
  (let [url "http://dbpedia.org/sparql"
        goal '(rdf/type dbpedia/Carneades ?x)
        subs '{?a "a" ?b "b"}
        triplestore-generator (generate-arguments-from-triplestore url)
        responses (generator/generate triplestore-generator goal subs)]
    (is (not (empty? responses)))
    (is (>= (count responses) 10))
    (is (not= (:substitutions (first responses)) subs))))

(deftest test-generate-arguments-from-triplestore-xsdstring-convertion
  (let [url "http://markos.man.poznan.pl/openrdf-sesame"
        repo "markos_test_sp2"
        goal '(http://www.markosproject.eu/ontologies/software#name ?x "org.apache.log4j")
        subs '{?a "a" ?b "b"}
        triplestore-generator (generate-arguments-from-triplestore url repo {})
        responses (generator/generate triplestore-generator goal subs)
        ]
    (is (not (empty? responses)))
    (is (>= (count responses) 5))
    (is (not= (:substitutions (first responses)) subs))))

(deftest test-generate-arguments-from-triplestore-with-engine
  (let [generators [(generate-arguments-from-triplestore "http://dbpedia.org/sparql")]
        graph (shell/argue (shell/make-engine 50 [] generators)
                           caes/caes
                           '(rdf/type dbpedia/Carneades ?x))]
    ;; (pprint graph)
    ;; (lacij/export graph "/tmp/carneades.svg")
    (is (>= (count (ag/arguments graph)) 10))))
