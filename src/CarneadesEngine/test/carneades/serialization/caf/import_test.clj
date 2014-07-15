(ns carneades.serialization.caf.import-test
  (:require [midje.sweet :refer :all]
            [clojure.pprint :refer [pprint]]
            [carneades.engine.statement :as s]
            [carneades.engine.argument :as a]
            [carneades.engine.argument-graph :as ag]
            [carneades.serialization.caf.export :as cafo]
            [carneades.serialization.caf.import :as cafi]
            [carneades.engine.argument-evaluation :refer [evaluate]]
            [carneades.engine.aspic :refer [aspic-grounded]]
            [taoensso.timbre :as timbre :refer [debug info spy]]))

(fact "(comp import export) is equivalent to identity"
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
                                              :de "Hochzeit"}})
            g (ag/enter-reference g {:title "ref1" :key "k1"})
            g (ag/enter-reference g {:title "ref2" :key "k2"})
            output (cafo/export g)
            tempfile (java.io.File/createTempFile "argumentgraph" ".xml")
            _ (spit tempfile output)
            g' (cafi/import tempfile)]
        (:header g') => (:header g)
        (:arguments-nodes g') => (:arguments-nodes g)
        (:statements-nodes g') => (:statements-nodes g)
        (:references g') => (:references g)
        g' => g))
