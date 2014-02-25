;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(defproject carneades/carneades-rest "1.0.0"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/data.json "0.1.1"]
                 [carneades/carneades-engine "2.0.2"]
                 [compojure "1.0.1" :exclusion [clojure]]
                 [org.clojure/java.jdbc "0.1.2"]
                 [ring/ring-servlet "1.0.1"]
                 [ring-middleware-format "0.2.3"]
                 [org.clojure/tools.logging "0.2.6"]]
  :plugins [[lein-ring "0.7.1"]]
  :ring {:handler carneades.web.service/carneades-web-service
         :init carneades.web.service/start}
  :min-lein-version "2.0.0"
  :description "This is a RESTful Web Service API for the Carneades argumentation engine."
  :cljsbuild {:builds []})
