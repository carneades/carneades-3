;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.policy-analysis.web.main
  (:use ring.adapter.jetty
        clojure.java.browse)
  (:require carneades.policy-analysis.web.routes-war))

(defn -main [& args]
  (let [port 8080
        url (str "http://localhost:" port)]
    (run-jetty #'carneades.policy-analysis.web.routes-war/carneades-webapp {:join? false :port 8080})
    (browse-url url)))


