(ns carneades.xml.caf.export-test
  (:require [midje.sweet :refer :all]
            [carneades.engine.aspic :refer [aspic-grounded]]
            [carneades.engine.statement :as s]
            [carneades.engine.argument :as a]
            [carneades.engine.argument-graph :as ag]
            [carneades.engine.argument-evaluation :refer [evaluate]]
            [taoensso.timbre :as timbre :refer [debug info spy]]
            [carneades.xml.caf.export :as caf]
            [carneades.xml.validation :refer [create-validation-fn]]
            [clojure.java.io :as io]))

(fact "The exported XML is conform to the schema."
      (let [wears-ring (s/make-statement :text {:en "Fred wears a ring."})
            married (s/make-statement :text {:en "Fred is married."} :atom '(married Fred))
            a1 (a/make-argument :id 'a1 :conclusion married :premises [(a/pm wears-ring)])
            g (-> (ag/make-argument-graph)
                  (ag/enter-arguments [a1])
                  (ag/accept [wears-ring]))
            g (evaluate aspic-grounded g)
            validator (create-validation-fn (.getPath (io/resource "test/schemas/CAF.xsd")))
            g (assoc g :header {:title "Marriage"
                                :description {:en "Wedding"
                                              :de "Hochzeit"
                                              :fr ""}})
            output (caf/export g)]
        (debug (validator output))
        (:right (validator output)) => true))
