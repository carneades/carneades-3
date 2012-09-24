(defproject carneades-web-service/carneades-web-service "1.0.0-SNAPSHOT" 
  :dependencies [[org.clojure/clojure "[1.4.0,]"]
                 [org.clojure/data.json "0.1.1"]
                 [carneades-engine "2.0.0-SNAPSHOT"]
                 [compojure "1.0.1" :exclusion [clojure]]
                 [org.clojure/java.jdbc "0.1.2"]
                 [ring/ring-servlet "1.0.1"]]
  :plugins [[lein-ring "0.7.1"]]
  :ring {:handler carneades.web.service/carneades-web-service}
  :min-lein-version "2.0.0"
  :description "This is a RESTful Web Service API for the Carneades argumentation engine.")
