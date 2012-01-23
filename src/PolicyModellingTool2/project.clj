(defproject policymodellingtool2 "0.1.0"
  :description "Policy Modelling Tool for the IMPACT Policy project"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [compojure "1.0.1"]
                 [hiccup "0.3.6"]
                 [enlive "1.0.0"]
                 [carneades-engine "1.0.0-SNAPSHOT"]
                 [org.clojars.pallix/mygengo "1.0.0"]
                 [org.clojure/data.json "0.1.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [carneades-mapcomponent "1.0.0-SNAPSHOT"]]
  :dev-dependencies [[lein-ring "0.5.0"]
                     [swank-clojure "1.4.0-SNAPSHOT"]]
  :ring {:handler impact.web.routes/app})
