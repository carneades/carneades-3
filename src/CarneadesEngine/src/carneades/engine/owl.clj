
(ns carneades.engine.owl
  (:require 
    [clojure.xml :as xml])
  (:use
    clojure.contrib.def
    ;clojure.contrib.profile
    carneades.engine.owl.reasoner
    carneades.engine.owl.rule
    carneades.engine.rule
    )
  (:import
    ;(com.clarkparsia.pellet.owlapiv3 PelletReasoner PelletReasonerFactory)
    (org.semanticweb.owlapi.apibinding OWLManager)
    (org.semanticweb.owlapi.model IRI MissingImportListener OWLOntologyIRIMapper)
    ;(org.semanticweb.owlapi.vocab OWLRDFVocabulary)
    ;(org.semanticweb.owlapi.util SimpleIRIMapper)
    (org.semanticweb.HermiT Reasoner$ReasonerFactory)
    (java.net URI)
    (java.io File)))


(defn owl?
  [url]
  (= (:tag (xml/parse url)) :rdf:RDF))

(defn- path->uri
  [path]
  (let [file (new File path)]
    (if (. file exists)
      (. file toURI)
      (new URI path))))

(defn- iri-to-file-mapper [prepath]
  (proxy [OWLOntologyIRIMapper] []
    (getDocumentIRI [ontIRI]
      ;(println "mapper in: ontURI:" ontIRI (.getScheme ontIRI))
      (if (= (.getScheme ontIRI) "file")
        (do
          ;(println "mapper map: no map")
          ontIRI)
        (let [ontURI (.toURI ontIRI),
              uriPath* (last (.split (.getPath ontURI) "/")),
              uriPath (if (= (.indexOf uriPath* ".") -1)
                        (str uriPath* ".owl")
                        uriPath*),
              iri (IRI/create (path->uri (str prepath uriPath)))]
          ;(println "mapper map:" ontIRI "->" iri)
          iri)))))

(defvar- missing-import-handler
  (proxy [MissingImportListener] []
    (importMissing [event]
      (let [uri (.getImportedOntologyURI event)]
        (println "!!! could not load ontology " uri "!!!")))))

(defn load-ontology
  ([path] (load-ontology path ""))
  ([path pre-path]
    (let [manager (OWLManager/createOWLOntologyManager),
          se (. manager setSilentMissingImportsHandling true)
          mih (. manager addMissingImportListener missing-import-handler),
          iri-map (. manager addIRIMapper (iri-to-file-mapper pre-path)),
          documentIRI (IRI/create (path->uri (str pre-path path))),
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
