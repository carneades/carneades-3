(defproject carneades-web-service "1.0.0-SNAPSHOT"
  :description "This is a RESTful Web Service API for the Carneades 
                argumentation engine."
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/data.json "0.1.1"]
                 [carneades-engine "1.0.0-SNAPSHOT"]
                 [compojure "0.6.4"]
                 [org.clojure/java.jdbc "0.1.1"]]
  :dev-dependencies [[swank-clojure "1.4.0-SNAPSHOT"]
                     [org.clojars.rayne/autodoc "0.8.0-SNAPSHOT"]
                     [lein-ring "0.4.5"]] 
  :ring {:handler carneades.web.service/carneades-web-service})