;; Copyright (c) 2010-2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.owl.import
  (:refer-clojure :exclude [import])
  (:require [carneades.project.fs :as project]
            [carneades.owl.owl :as o]
            [carneades.owl.import-axioms :as axioms]
            [carneades.owl.import-language :as language]))

(defn import
  "Imports an OWL ontology serialized in RDF/XML. Returns a map with keys :axioms and :language."
  ([url]
     (import url "."))
  ([url importdir]
     (let [ontology (o/load-ontology url importdir)]
       {:axioms (axioms/ontology->schemes ontology)
        :language (language/ontology->language ontology)})))

(defn import-from-project
  "Imports an ontology from a Carneades project."
  [project ontology]
  (import (project/absolute-ontology-path project ontology)
          (project/absolute-theory-dir-path project)))
