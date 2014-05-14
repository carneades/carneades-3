;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.engine.triplestore-test
  (:require [carneades.engine.argument-generator :as generator]
            [carneades.engine.shell :as shell]
            [carneades.engine.caes :as caes]
            [carneades.engine.argument-graph :as ag]
            [carneades.maps.lacij :as lacij]
            [midje.sweet :refer :all]
            [clojure.pprint :refer :all]
            [carneades.engine.triplestore :refer :all]))

(comment
  
 (fact "Grounded goal are accepted by the trieplstore generator."
       (let [url "http://dbpedia.org/sparql"
             goal '(rdf/type dbpedia/Carneades dbpedia-owl/Philosopher)
             subs '{?a "a" ?b "b"}
             triplestore-generator (generate-arguments-from-triplestore url)
             responses (generator/generate triplestore-generator goal subs)]
         (expect (not (empty? responses)) => true)
         (expect (count responses) => 1)
         (expect (-> (first responses) :argument :conclusion) => goal)
         (expect (-> (first responses) :substitutions) => subs)))

 (fact "Goal containing IRI are accepted by the triplestore generator."
       (let [url "http://dbpedia.org/sparql"
             goal '(rdf/type
                    http://dbpedia.org/resource/Carneades
                    http://dbpedia.org/ontology/Philosopher)
             subs '{?a "a" ?b "b"}
             triplestore-generator (generate-arguments-from-triplestore url)
             responses (generator/generate triplestore-generator goal subs)]
         (expect (not (empty? responses)) => true)
         (expect (count responses) => 1)
         (expect (-> (first responses) :argument :conclusion) => goal)
         (expect (-> (first responses) :substitutions) => subs )))

 (fact "Goal containg variables are accepted by the triplestore generator."
       (let [url "http://dbpedia.org/sparql"
             goal '(rdf/type dbpedia/Carneades ?x)
             subs '{?a "a" ?b "b"}
             triplestore-generator (generate-arguments-from-triplestore url)
             responses (generator/generate triplestore-generator goal subs)]
         (expect (not (empty? responses)) => true)
         (expect (>= (count responses) 10) => true)
         (expect (:substitutions (first responses)) =not=> subs)))

 (fact "Goal representing concept are accepted by the triplestore generator."
       (let [url "http://dbpedia.org/sparql"
             goal '(http://dbpedia.org/class/yago/HellenisticEraPhilosophersInAthens ?x)
             subs '{?a "a" ?b "b"}
             triplestore-generator (generate-arguments-from-triplestore url)
             responses (generator/generate triplestore-generator goal subs)]
         (expect (not (empty? responses)) => true)
         (expect (>= (count responses) 10) => true)
         (expect (:substitutions (first responses)) =not=> subs)))

 (fact "Arguments can be generated from a triplestore generator."
       (let [generators [(generate-arguments-from-triplestore "http://dbpedia.org/sparql")]
             graph (shell/argue (shell/make-engine 50 [] generators)
                                caes/caes
                                '(rdf/type dbpedia/Carneades ?x))]
         ;; (pprint graph)
         ;; (lacij/export graph "/tmp/carneades.svg")
         (expect (>= (count (ag/arguments graph)) 10) => true))))

(fact "Convertion from string to xsd:string are performed by the triplestore generator."
      (let [url "http://markos.man.poznan.pl/openrdf-sesame"
            repo "markos_test_26-07-2013"
            goal '(http://www.markosproject.eu/ontologies/software#name ?x "org.apache.log4j")
            subs '{?a "a" ?b "b"}
            triplestore-generator (generate-arguments-from-triplestore url repo {})
            responses (generator/generate triplestore-generator goal subs)
            ]
        (expect (not (empty? responses)) => true)
        (expect (>= (count responses) 5) => true)
        (expect (:substitutions (first responses)) =not=> subs)))


