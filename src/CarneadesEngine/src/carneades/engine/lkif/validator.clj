
(ns carneades.engine.lkif.validator
  ;(:require )
  (:use 
    carneades.config.reader
    clojure.java.io)
  (:import 
    javax.xml.XMLConstants
    javax.xml.validation.SchemaFactory 
    javax.xml.transform.stream.StreamSource
    org.xml.sax.SAXException    
    )
  )

(defn validate-xsd
  "Validates an lkif file against the schema file

   Throws: SAXException if lkif is not valid"
  [lkif-path schema-path]
  (let [factory (. SchemaFactory newInstance (. XMLConstants W3C_XML_SCHEMA_NS_URI)),
        schema-file (file schema-path),
        schema (. factory newSchema schema-file),
        validator (. schema newValidator),
        lkif-file (new StreamSource lkif-path)]
    (. validator validate lkif-file)
    true))

(defn valid-xsd?
  "Validates an lkif lkif file against a schema file.
   Returns true if valid, false otherwise"
  [lkif-path scheme-path]
  (try (validate-xsd lkif-path scheme-path) (catch SAXException e false)))