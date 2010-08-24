;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.pellet
  (:require     
    [carneades.engine.argument-search :as as] ; for testing only
    )
  (:use    
    carneades.engine.statement
    carneades.engine.unify
    [carneades.engine.argument :as arg])
  (:import
    (com.clarkparsia.pellet.owlapiv3 PelletReasoner PelletReasonerFactory)
    (org.semanticweb.owlapi.apibinding OWLManager)
    (org.semanticweb.owlapi.model IRI OWLLogicalAxiom AxiomType ClassExpressionType MissingImportListener OWLOntologyIRIMapper)
    (org.semanticweb.owlapi.vocab OWLRDFVocabulary)
    (org.semanticweb.owlapi.reasoner OWLReasoner OWLReasonerFactory)
    (org.semanticweb.owlapi.util SimpleIRIMapper)
    (java.net URI)
    (java.io File))
  )

(defn path->uri
  [path]
  (let [file (new File path)]
    (if (. file exists)
      (. file toURI)
      (new URI path))))


(defn class-instances-to-responses
  [reasoner manager wff subs path]
  ;(println "finding instances of class" (first wff))
  (let [cname (str (first wff)),
        clazz (. (. manager getOWLDataFactory) getOWLClass (IRI/create cname)),
        insts (. (. reasoner getInstances clazz false) getFlattened)]
    ;(println "# of instances found:" (count insts))
    (map (fn [i]
           (let [c (list (first wff) (symbol (. i toStringID))),
                 subs2 (unify c wff subs)]
             (as/response
               (or subs2 subs)
               (arg/argument
                 (gensym "a")
                 :pro
                 c
                 (list (arg/am (list 'valid (symbol path))))
                 "Pellet"))))
      insts)))

(defn single-class-instance-to-responses
  [reasoner manager wff subs path]
  ;(println "finding instances of class" (first wff))
  (let [cname (str (first wff)),
        clazz (. (. manager getOWLDataFactory) getOWLClass (IRI/create cname)),
        iname (str (second wff)),
        ind (. (. manager getOWLDataFactory) getOWLNamedIndividual (IRI/create iname)),
        insts (map (memfn toStringID) (. (. reasoner getInstances clazz false) getFlattened))]
    ;(println "# of instances found:" (count insts))
    (if (some #{iname} insts)
      (let []
        ;(println iname "is instance of" cname)
        (list (as/response
              subs
              (arg/argument
                (gensym "a")
                :pro
                wff
                (list (arg/am (list 'valid (symbol path))))
                "Pellet"))))
      (let []
        ;(println iname "is not an instance of" cname)
        ;(println "instances found: insts")
        nil))))

(defn property-instances-to-responses
  [reasoner manager wff subs path ontology]
  ;(println "finding instances of property:" (first wff))
  ;(println "subject is:" (second wff))
;  (println "property is an:" (cond
;                    (. ontology containsDataPropertyInSignature (IRI/create (str (first wff)))) "data-property",
;                    (. ontology containsObjectPropertyInSignature (IRI/create (str (first wff))) true) "object-property"))
  (let [pname (str (first wff)),
        prop-type (cond
                    (. ontology containsDataPropertyInSignature (IRI/create pname)) :data,
                    (. ontology containsObjectPropertyInSignature (IRI/create pname) true) :object),
        prop (condp = prop-type
               :data (. (. manager getOWLDataFactory) getOWLDataProperty (IRI/create pname)),
               :object (. (. manager getOWLDataFactory) getOWLObjectProperty (IRI/create pname))),
        iname (str (second wff)),
        ind (. (. manager getOWLDataFactory) getOWLNamedIndividual (IRI/create iname)),
        insts (condp = prop-type
                :data (map (memfn getLiteral) (. reasoner getDataPropertyValues ind prop)),
                :object (map (memfn toStringID) (. (. reasoner getObjectPropertyValues ind prop) getFlattened)))]
;    (println "is contained prop?:" (or (.containsObjectPropertyInSignature ontology (IRI/create pname) true)
;                                       (.containsDataPropertyInSignature ontology (IRI/create pname) true)))
;    (println "is contained ind?:" (.containsIndividualInSignature ontology (IRI/create iname) true))
    ;(println "# of instances found:" (count insts))
;    (println "prop" prop)
;    (println "ind" ind)
;    (println "nodeSet:" (. reasoner getObjectPropertyValues ind prop))
;    (println "singleton?:" (.isSingleton (. reasoner getObjectPropertyValues ind prop)))
;    (println "bottom-singleton?:" (.isBottomSingleton (. reasoner getObjectPropertyValues ind prop)))
;    (println "empty?:" (.isEmpty (. reasoner getObjectPropertyValues ind prop)))
;    (println "nodes:" (.size (.getNodes (. reasoner getObjectPropertyValues ind prop))))
;    (println "flattened:" (.size (.getFlattened (. reasoner getObjectPropertyValues ind prop))))
;      (println "direct imports" (count (.getDirectImports manager ontology)))
;      (println "processing goal:" wff)
;      (println "consistent ontology?:" (.isConsistent reasoner))
;    (println "number of axioms:" (.getLogicalAxiomCount ontology))
;      (println "number of axioms for prop:" (count (.getAxioms ontology prop)))
;    (println "number of axioms for ind:" (count (.getAxioms ontology ind)))
;      ;(println "axioms:" (map (memfn toString) (.getLogicalAxioms ontology)))
;      ;(println "object-properties:" (map (memfn toString) (.getObjectPropertiesInSignature ontology true)))
;      (println "number of object-properties:" (count (.getObjectPropertiesInSignature ontology true)))
;      (println "number of data-properties:" (count (.getDataPropertiesInSignature ontology true)))
;      ;(println "individuals:" (map (memfn toString) (.getIndividualsInSignature ontology true)))
;      (println "number of individuals:" (count (.getIndividualsInSignature ontology true)))
;      ;(println "classes:" (map (memfn toString) (.getFlattened (.getSubClasses reasoner (. (. manager getOWLDataFactory) getOWLThing) false))))
;      (println "number of classes:" (count (.getFlattened (.getSubClasses reasoner (. (. manager getOWLDataFactory) getOWLThing) false))))
    (map (fn [i]
           (let [c (list (first wff) (second wff) (symbol i)),
                 subs2 (unify c wff subs)]
             (as/response
               subs2
               (arg/argument
                 (gensym "a")
                 :pro
                 c
                 (list (arg/am (list 'valid (symbol path))))
                 "Pellet"))))
      insts)))

(defn single-property-instance-to-responses
  [reasoner manager wff subs path ontology]
  ;(println "finding instances of property" (first wff))
  ;(println "property is" (cond
  ;                  (. ontology containsDataPropertyInSignature (IRI/create (str (first wff)))) :data,
  ;                  (. ontology containsObjectPropertyInSignature (IRI/create (str (first wff))) true) :object))
  (let [pname (str (first wff)),
        prop-type (cond
                    (. ontology containsDataPropertyInSignature (IRI/create pname)) :data,
                    (. ontology containsObjectPropertyInSignature (IRI/create pname) true) :object),
        prop (condp = prop-type
               :data (. (. manager getOWLDataFactory) getOWLDataProperty (IRI/create pname)),
               :object (. (. manager getOWLDataFactory) getOWLObjectProperty (IRI/create pname))),
        iname (str (second wff)),
        iname2 (str (nth wff 2)),
        ind (. (. manager getOWLDataFactory) getOWLNamedIndividual (IRI/create iname)),
        insts (condp = prop-type
                :data (map (memfn getLiteral) (. reasoner getDataPropertyValues ind prop)),
                :object (map (memfn toStringID) (. (. reasoner getObjectPropertyValues ind prop) getFlattened)))]
    ;(println "# of instances found:" (count insts))
    (if (some #{iname2} insts)
      (let []
        ;(println iname "is instance of" pname)
        (list (as/response
              subs
              (arg/argument
                (gensym "a")
                :pro
                wff
                (list (arg/am (list 'valid (symbol path))))
                "Pellet"))))
      (let []
        ;(println iname2 "is not an instance of" pname)
        ;(println "instances found:" insts)
        nil))))


(defn iri-to-file-mapper [prepath]
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

(def missing-import-handler
  (proxy [MissingImportListener] []
    (importMissing [event]
      (let [uri (.getImportedOntologyURI event)]
        (println "!!! could not load ontology " uri "!!!")))))

(defn generate-arguments-from-owl 
  ([path] (generate-arguments-from-owl path ""))
  ([path pre-path]
  (fn [subgoal state]
    ;(println "start:" (str pre-path path))
    (let [manager (OWLManager/createOWLOntologyManager),
          se (. manager setSilentMissingImportsHandling true)
          mih (. manager addMissingImportListener missing-import-handler),
          iri-map (. manager addIRIMapper (iri-to-file-mapper pre-path)),
          documentIRI (IRI/create (path->uri (str pre-path path))),          
          ontology (. manager loadOntology documentIRI),
          reasoner (. (PelletReasonerFactory/getInstance) createReasoner ontology)
          subs (:substitutions state),
          wff (subs (statement-wff subgoal))]
      ;(println "ontology loaded")
      ;(println "direct imports" (count (.getDirectImports manager ontology)))
      ;(println "processing goal:" wff)
;      (println "consistent ontology?:" (.isConsistent reasoner))
;      (println "number of axioms:" (.getLogicalAxiomCount ontology))
;      ;(println "axioms:" (map (memfn toString) (.getLogicalAxioms ontology)))
;      ;(println "object-properties:" (map (memfn toString) (.getObjectPropertiesInSignature ontology true)))
;      (println "number of object-properties:" (count (.getObjectPropertiesInSignature ontology true)))
;      (println "number of data-properties:" (count (.getDataPropertiesInSignature ontology true)))
;      ;(println "individuals:" (map (memfn toString) (.getIndividualsInSignature ontology true)))
;      (println "number of individuals:" (count (.getIndividualsInSignature ontology true)))
;      ;(println "classes:" (map (memfn toString) (.getFlattened (.getSubClasses reasoner (. (. manager getOWLDataFactory) getOWLThing) false))))
;      (println "number of classes:" (count (.getFlattened (.getSubClasses reasoner (. (. manager getOWLDataFactory) getOWLThing) false))))
      (. reasoner prepareReasoner) ; TODO : what happens here?
      (cond
        (and
          (= (count wff) 2)
          (variable? (second wff))) (class-instances-to-responses reasoner manager wff subs path),
        (and
          (= (count wff) 2)
          (not (variable? (second wff)))) (single-class-instance-to-responses reasoner manager wff subs path),
        (and
          (= (count wff) 3)
          (variable? (nth wff 2))) (property-instances-to-responses reasoner manager wff subs path ontology)
        (and
          (= (count wff) 3)
          (not (variable? (nth wff 2)))) (single-property-instance-to-responses reasoner manager wff subs path ontology))))))


;(def p1 "C:\\Users\\stb\\Documents\\Carneades Project\\carneades\\examples\\open-source-licensing\\oss-licenses.owl")
;(def p2 "C:\\Users\\stb\\Documents\\Carneades Project\\carneades\\examples\\open-source-licensing\\impact-licensing.owl")
;(def p3 "http://owl.man.ac.uk/2006/07/sssw/people.owl")
;(def cn "http://owl.man.ac.uk/2006/07/sssw/people#has_pet")
;(def cn2 "http://owl.man.ac.uk/2006/07/sssw/people#kevin")
;(def cs (symbol cn))
;(def cs2 (symbol cn2))
;(def goal (list cs cs2 '?x))
;(def s1 (as/initial-state goal arg/*empty-argument-graph*))
;(def manager (OWLManager/createOWLOntologyManager))
;(def se (. manager setSilentMissingImportsHandling true)) ; TODO : handle imports
;(def documentIRI (IRI/create (path->uri p3)))
;(def ontology (. manager loadOntology documentIRI)) ; TODO imports don't work
;(def reasoner (. (PelletReasonerFactory/getInstance) createReasoner ontology))

;(def c1 (. (. manager getOWLDataFactory) getOWLClass (IRI/create cn)))
;(def c2 (. (. manager getOWLDataFactory) getOWLClass (IRI/create cn2)))

;((generate-arguments-from-owl p3) s1 goal)

