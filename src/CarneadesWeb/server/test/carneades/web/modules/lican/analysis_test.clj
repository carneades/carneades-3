(ns carneades.web.modules.lican.analysis-test
  (:require [midje.sweet :refer :all]
            [carneades.web.modules.lican.analysis :as analysis]
            [carneades.maps.lacij :refer [export]]
            [taoensso.timbre :as timbre :refer [debug info spy]]))

(fact "It is not possible to use a GPL library from an Apache software."
      (let [l (analysis/find-sofware-entities-with-compatible-licenses
               nil
               "http://www.markosproject.eu/ontologies/oss-licenses#Apache-2.0"
               ["http://www.markosproject.eu/ontologies/software#dynamicallyLinkedEntity"]
               ["http://markosproject.eu/kb/Library/549"] ;; GPL lib
               )]
        l => ()))

(fact "It is possible to use a GPL library from a GPL software."
      (let [l (analysis/find-sofware-entities-with-compatible-licenses
               nil
               "http://www.markosproject.eu/ontologies/oss-licenses#GPL-2.0"
               ["http://www.markosproject.eu/ontologies/software#dynamicallyLinkedEntity"]
               ["http://markosproject.eu/kb/Library/549"] ;; GPL lib
               )]
        l => '("http://markosproject.eu/kb/Library/549")))
