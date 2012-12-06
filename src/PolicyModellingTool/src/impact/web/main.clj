;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns impact.web.main
  (:use ring.adapter.jetty
        clojure.java.browse)
  (:require impact.web.routes-war))

(defn -main [& args]
  (let [port 8080
        url (str "http://localhost:" port)]
    (run-jetty #'impact.web.routes-war/impact-app {:join? false :port 8080})
    (browse-url url)))


