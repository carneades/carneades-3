(ns carneades.owl.import-language-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.pprint :refer :all]
            [carneades.owl.import-language :as language]
            [carneades.owl.owl :as o]
            [carneades.engine.utils :as utils]))

(deftest test-ontology->language
  (let [ont (o/load-ontology (io/resource "test/ontologies/markos-licenses.owl"))]
    (is '{http://www.markosproject.eu/ontologies/licenses#LicenseAssessment
          {:symbol
           http://www.markosproject.eu/ontologies/licenses#LicenseAssessment,
           :category nil,
           :askable false,
           :hint {},
           :followups [],
           :text {:en "assessment"}},
          http://www.markosproject.eu/ontologies/licenses#Work
          {:symbol http://www.markosproject.eu/ontologies/licenses#Work,
           :category nil,
           :askable false,
           :hint {},
           :followups [],
           :text {:en "work"}},
          http://www.markosproject.eu/ontologies/licenses#assessment
          {:symbol http://www.markosproject.eu/ontologies/licenses#assessment,
           :min 1,
           :max 1,
           :type
           http://www.markosproject.eu/ontologies/licenses#LicenseAssessment,
           :askable false,
           :default "",
           :forms {},
           :category "",
           :hint {:en "some hint"},
           :followups []},
          http://www.markosproject.eu/ontologies/licenses#argumentation
          {:symbol
           http://www.markosproject.eu/ontologies/licenses#argumentation,
           :min 1,
           :max 1,
           :type xsd:string,
           :askable false,
           :default "",
           :forms {},
           :category "",
           :hint
           "",
           :followups [],
           :text {:en "argumentation"}},
          http://www.markosproject.eu/ontologies/licenses#aWork
          {:symbol http://www.markosproject.eu/ontologies/licenses#aWork,
           :text {:en "le travail"}}}
        (utils/unrecordify (language/ontology->language ont)))))
