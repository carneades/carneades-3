;;; Copyright (c) 2010-2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.owl.import
  (:refer-clojure :exclude [import])
  (:require [carneades.project.admin :as project]
            [carneades.owl.owl :as o]
            [carneades.owl.import-axioms :as axioms]))

(defn import
  "Imports an OWL ontology serialized in RDF/XML. Returns a map with keys :axioms and :language."
  ([url]
     (import url "."))
  ([url importdir]
     (let [ontology (o/load-ontology url importdir)]
       {:axioms (axioms/ontology->schemes ontology)})))

(defn import-from-project
  "Imports an ontology from a Carneades project."
  [project ontology]
  (import (project/absolute-ontology-path project ontology)
          (project/absolute-theory-dir-path project)))
