(ns carneades.serialization.aif.import-test
  (:require [midje.sweet :refer :all]
            [carneades.engine.uuid :as id]
            [carneades.engine.statement :as s]
            [carneades.engine.argument :as a]
            [carneades.engine.argument-graph :as ag]
            [taoensso.timbre :as timbre :refer [debug info spy]]
            [carneades.serialization.aif.import :as import]
            [carneades.serialization.aif.export :as export])
  (:import [java.io StringWriter StringReader]))


(fact "The example argument graph is translated into AIF."
      (let [contract (s/make-statement :text {:en "There is a contract."})
            minor (s/make-statement :text {:en "The person who made the offer is a minor."})
            writing (s/make-statement :text {:en "The offer was made in writing."})
            agreement (s/make-statement :text {:en "The parties have reached an agreement."})
            real-estate (s/make-statement :text {:en "The agreement regards a sale of real estate."})
            email (s/make-statement :text {:en "The agreement was reached via email."})
            deed (s/make-statement :text {:en "A deed was attached to the email."})
            a1 (a/make-argument :conclusion contract
                                :premises [(a/make-premise :statement minor
                                                           :positive false)
                                           (a/make-premise :statement agreement)])
            a2 (a/make-argument :pro false
                                :conclusion contract
                                :premises [(a/make-premise :statement writing
                                                           :positive false)
                                           (a/make-premise :statement real-estate)])
            a3 (a/make-argument :pro false
                                :conclusion writing
                                :premises [(a/make-premise :statement email)])
            a4 (a/make-argument :conclusion agreement
                                :premises [(a/make-premise :statement deed)])
            a5 (a/make-argument :conclusion real-estate
                                :premises [(a/make-premise :statement deed)])
            g1 (-> (ag/make-argument-graph)
                   (ag/enter-arguments [a1 a2 a3 a4 a5]))
            aif1 (str (with-open [w (StringWriter.)]  
                       (export/argument-graph->aif g1 :en w)))
            g2 (import/aif->argument-graph (StringReader. aif1) :en)
            aif2 (str (with-open [w (StringWriter.)]  
                       (export/argument-graph->aif g2 :en w)))]
        ;; (debug g2)
        (debug aif2)
        true => true))  ;; dummy test. Interested only in the debugging output.
        
