;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.owl
  (:require [clojure.xml :as xml])
  (:use clojure.contrib.def
        carneades.engine.owl.reasoner
        carneades.engine.owl.rule
        carneades.engine.rule)
  (:import (java.net URI)
           (java.io File)
           (org.semanticweb.owlapi.apibinding OWLManager)
           (org.semanticweb.owlapi.model IRI MissingImportListener OWLOntologyIRIMapper OWLClass)
           (org.semanticweb.HermiT Reasoner$ReasonerFactory)))

(defn owl?
  [url]
  (= (:tag (xml/parse url)) :rdf:RDF))

(defn- path->uri
  [path prepath]
  (let [file (new File path)]
    (if (. file exists)
      (. file toURI)
      (if prepath
        (path->uri (str prepath path) nil)
        (new URI path)))))

(defn- iri-to-file-mapper [prepath]
  (proxy [OWLOntologyIRIMapper] []
    (getDocumentIRI [ontIRI]
      ;(println "mapper in: ontURI:" ontIRI (.getScheme ontIRI))
      (if (or (nil? prepath) (= (.getScheme ontIRI) "file"))
        (do
          ;(println "mapper map: no map")
          ontIRI)
        (let [ontURI (.toURI ontIRI),
              uriPath* (last (.split (.getPath ontURI) "/")),
              uriPath (if (= (.indexOf uriPath* ".") -1)
                        (str uriPath* ".owl")
                        uriPath*),
              iri (IRI/create (path->uri uriPath prepath))]
          ;(println "mapper map:" ontIRI "->" iri)
          iri)))))

(defvar- missing-import-handler
  (proxy [MissingImportListener] []
    (importMissing [event]
      (let [uri (.getImportedOntologyURI event)]
        (println "!!! could not load ontology " uri "!!!")))))

(defn load-ontology
  ([path] (load-ontology path nil))
  ([path pre-path]
    ;(println "loading ontology" path pre-path)
    (let [manager (OWLManager/createOWLOntologyManager),
          se (. manager setSilentMissingImportsHandling true)
          mih (. manager addMissingImportListener missing-import-handler),
          iri-map (. manager addIRIMapper (iri-to-file-mapper pre-path)),          
          documentIRI (IRI/create (path->uri path pre-path)),
          ontology (. manager loadOntology documentIRI),
          reasoner (. (new Reasoner$ReasonerFactory) createReasoner ontology)]
      (. reasoner prepareReasoner)
      ;(println "ontology loaded")
      ;(println "direct imports" (count (.getDirectImports manager ontology)))
      ;(println "processing goal:" wff)
      ;      (println "consistent ontology?:" (.isConsistent reasoner))
      ;      (println "number of axioms:" (.getLogicalAxiomCount ontology))
      ;      (println "axioms:" (map (memfn toString) (.getLogicalAxioms ontology)))
      ;      (println "object-properties:" (map (memfn toString) (.getObjectPropertiesInSignature ontology true)))
      ;      (println "number of object-properties:" (count (.getObjectPropertiesInSignature ontology true)))
      ;      (println "number of data-properties:" (count (.getDataPropertiesInSignature ontology true)))
      ;      (println "individuals:" (map (memfn toString) (.getIndividualsInSignature ontology true)))
      ;      (println "number of individuals:" (count (.getIndividualsInSignature ontology true)))
      ;      (println "classes:" (map (memfn toString) (.getFlattened (.getSubClasses reasoner (. (. manager getOWLDataFactory) getOWLThing) false))))
      ;      (println "number of classes:" (count (.getFlattened (.getSubClasses reasoner (. (. manager getOWLDataFactory) getOWLThing) false))))
       {:ontology ontology, :reasoner reasoner})))

(defn generate-arguments-from-owl
  ([ontology type]
    (generate-arguments-from-owl ontology type '()))
  ([ontology type optionals]
    (and (:ontology ontology)
      (condp = type
          :reasoner (generate-arguments-from-reasoner (:ontology ontology) (:reasoner ontology)),
        :rule (generate-arguments-from-rules (map-ontology (:ontology ontology) optionals) '()),
        (throw (Exception. "Invalid type value for owl generator"))))))

(defn individuals [ontology]
  "returns all individuals in the ontology as a list of (class individual)"
  (mapcat (fn [class]
            (let [individuals (.getIndividuals class ontology)
                  sid (symbol (.toStringID class))]
              (prn (bean class))
              (if (not (empty? individuals))
                (map (fn [individual]
                       (list sid (symbol (.toStringID individual))))
                     individuals)
                ())))
          (.getClassesInSignature ontology)))

(defn create-iri [s]
  (IRI/create s))

(defn classes [ontology iri]
  (seq (filter #(do (prn "obj =") (prn %) (instance? OWLClass %))
               (.getEntitiesInSignature ontology iri))))

(defn root-ontology [reasoner]
  (.getRootOntology reasoner))

(defn instances [reasoner class]
  "returns instances of class"
  (seq (.getFlattened (.getInstances reasoner class false))))

