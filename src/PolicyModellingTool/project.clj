(defproject policymodellingtool/policymodellingtool "1.0.0-SNAPSHOT" 
  :dependencies [[org.clojure/clojure "[1.4.0,]"]
                 [org.clojure/data.json "0.1.1"]
                 [compojure "1.0.1" :exclusion [clojure]]
                 [hiccup "0.3.6"]
                 [enlive "1.0.0"]
                 [ring/ring-servlet "1.0.1"]
                 [carneades-engine "2.0.0-SNAPSHOT"]
                 [carneades-web-service "1.0.0-SNAPSHOT"]
                 [org.clojars.pallix/mygengo "1.0.0"]
                 [lein-ring "0.5.4"]]
  :plugins [[lein-ring "0.7.1"]]
  :ring {:handler impact.web.routes-dev/impact-app}
  :min-lein-version "2.0.0"
  :description "Policy Modelling Tool for the IMPACT Policy project")
