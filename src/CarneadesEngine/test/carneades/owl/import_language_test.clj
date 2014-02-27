;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.owl.import-language-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.pprint :refer :all]
            [carneades.owl.import-language :as language]
            [carneades.owl.owl :as o]
            [carneades.engine.utils :as utils]
            [midje.sweet :refer :all]))

(fact "The annotation of an ontology can be converted into a language"
      (let [ont (o/load-ontology (io/resource "test/ontologies/markos-licenses.owl"))]
        (expect (utils/unrecordify (language/ontology->language ont)) =>
            '{http://www.markosproject.eu/ontologies/licenses#aWork
              {:symbol http://www.markosproject.eu/ontologies/licenses#aWork,
               :text {:en "le travail"}},
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
               :hint "",
               :followups [],
               :text {:en "argumentation"}},
              http://www.markosproject.eu/ontologies/licenses#Work
              {:symbol http://www.markosproject.eu/ontologies/licenses#Work,
               :category nil,
               :askable false,
               :hint {},
               :followups [],
               :text {:en "work"}},
              http://www.markosproject.eu/ontologies/licenses#LicenseAssessment
              {:symbol
               http://www.markosproject.eu/ontologies/licenses#LicenseAssessment,
               :category nil,
               :askable false,
               :hint {},
               :followups [],
               :text {:en "assessment"}},
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
               :followups []}
              http://www.markosproject.eu/ontologies/licenses#template
              {:askable false, :category "", :default "", :followups [], :forms {},
               :hint {:en "Hint for template"},
               :max 1, :min 1,
               :symbol http://www.markosproject.eu/ontologies/licenses#template,
               :type http://www.markosproject.eu/ontologies/licenses#CopyrightLicenseTemplate}})))

(fact "A language can be constructed from an ontology providing annotations for another."
      (let [ont (o/load-ontology (io/resource "test/ontologies/markos-licenses-annotated.owl"))]
        (expect (utils/unrecordify (language/ontology->language ont)) =>
                '{http://www.markosproject.eu/ontologies/licenses#template
                  {:symbol http://www.markosproject.eu/ontologies/licenses#template,
                   :default "",
                   :forms {:en {:positive "%s is licensed with %s"}},
                   :followups [],
                   :max 1,
                   :type
                   http://www.markosproject.eu/ontologies/licenses#CopyrightLicenseTemplate,
                   :hint "",
                   :askable false,
                   :min 1,
                   :category ""}})))
