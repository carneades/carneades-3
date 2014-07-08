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
            [clojure.java.io :as io]
            [clojure.data.zip.xml :refer [attr text xml->]]
            [clojure.zip :as z]
            [clojure.xml :as xml]))

(fact "The exported XML is conform to the schema."
      (let [wears-ring (s/make-statement :text {:en "Fred wears a ring."}
                                         :header {:description {:en "A long
            description from Fred wearing a ring."}})
            married (s/make-statement :text {:en "Fred is
            married."} :atom '(married Fred)) a1 (a/make-argument :id
            'a1 :conclusion married :premises [(a/pm wears-ring)])
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

(fact "The generated XML contains the relevant information."
      (let [wears-ring (s/make-statement :text {:en "Fred wears a ring."}
                                         :header {:description {:en "A long description from Fred wearing a ring."}
                                                  :creator "John"})
            married (s/make-statement :text {:en "Fred is married."} :atom '(married Fred))
            a1 (a/make-argument :id 'a1 :conclusion married :premises [(a/pm wears-ring)])
            g (-> (ag/make-argument-graph)
                  (ag/enter-arguments [a1])
                  (ag/accept [wears-ring]))
            g (evaluate aspic-grounded g)
            g (assoc g :header {:title "Marriage"
                                :description {:en "Wedding"
                                              :de "Hochzeit"
                                              :fr ""}})
            get-stmt (fn [id] (into {} (ag/get-statement-node g id)))
            n1 (get-stmt wears-ring)
            n1attrs (select-keys n1 [:id :main :standard :value :weight])
            n2 (get-stmt married)
            n2attrs (select-keys n2 [:id :main :standard :value :weight])
            output (caf/export g)
            xml (xml/parse (io/input-stream (into-array Byte/TYPE output)))
            zipper (z/xml-zip xml)
            stmts-nodes (map z/node (xml-> zipper :statements :statement))
            node1 (first stmts-nodes)
            attrs1 (:attrs node1)
            node2 (second stmts-nodes)
            attrs2 (:attrs node2)]
        (debug output)
        (if (= (symbol (:id attrs1)) (:id n1attrs))
          (do
            (:main attrs1) => (str (:main n1attrs))
            (:weight attrs1) => (str (:weight n1attrs))
            (:standard attrs1) => (.toUpperCase (name (:standard n1attrs)))
            (:value attrs1) => (str (:value n1attrs))
            (let [md (:attrs (first (:content node1)))]
              md => (dissoc (:header n1) :description)))
          ;; else
          (do
            (:main attrs2) => (str (:main n2attrs))
            (:weight attrs2) => (str (:weight n2attrs))
            (:standard attrs2) => (.toUpperCase (name (:standard n2attrs)))
            (:value attrs2) => (str (:value n2attrs))))))
