;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.handlers.suggestions
  (:use carneades.engine.statement
        [carneades.engine.rule :only (predicate condition-statement)])
  (:require [carneades.engine.owl :as owl]))

(defn possible-individuals-statements [literal reasoners]
  (prn "possible-individuals-statements")
  (let [owl-symbol (predicate literal)
        statement (condition-statement literal)
        iri (owl/create-iri (str owl-symbol))
        nb-args (count (term-args statement))]
    (prn "owl-symbol =")
    (prn owl-symbol)
    (prn "term args")
    (prn (term-args statement))
    (prn "reasoners =")
    (prn reasoners)
    (doall
     (mapcat (fn [reasoner]
               (let [ontology (owl/root-ontology reasoner)]
                 (condp = nb-args
                     1 (let [class (first (owl/classes ontology iri))]
                         (if (not (nil? class))
                           (map (fn [individual]
                                  (list owl-symbol (symbol (.toStringID individual))))
                                (owl/instances reasoner class))
                           ()))
                    
                     2 (mapcat #(owl/instances-with-property % owl-symbol) reasoners)
                     
                     ())))
             reasoners))))