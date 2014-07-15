(ns carneades.web.modules.lican.analysis-test
  (:require [midje.sweet :refer :all]
            [carneades.web.modules.lican.analysis :as analysis]
            [carneades.maps.lacij :refer [export]]
            [taoensso.timbre :as timbre :refer [debug info spy]]))

(fact "Blabla"
      (let [g (analysis/find-sofware-entities-with-compatible-licenses
               nil
               "http://www.markosproject.eu/ontologies/oss-licenses#GPL-2.0"
               ["http://www.markosproject.eu/ontologies/software#dynamicallyLinkedEntity"]
               [""])]
        (debug g)
        ;; (export g "/tmp/g1.svg")
        ))
