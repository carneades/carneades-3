(defproject carneades-web-service "1.0.0-SNAPSHOT"
  :description "This is a RESTful Web Service API for the Carneades 
                argumentation engine."
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/data.json "0.1.1"]
                 [carneades-engine "2.0.0-SNAPSHOT"]
                 [compojure "1.0.1" :exclusion [clojure]]
                 [ring/ring-servlet "1.0.1"]
                 [org.clojure/java.jdbc "0.1.1"]]
  :dev-dependencies [[lein-ring "0.4.5"]] 
  :ring {:handler carneades.web.service/carneades-web-service})