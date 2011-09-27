(defproject policymodellingtool2 "0.1.0"
  :description "Policy Modelling Tool for the IMPACT Policy project"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [compojure "0.6.4"]
                 [hiccup "0.3.6"]
                 [enlive "1.0.0"]
                 [carneades-engine "1.0.0-SNAPSHOT"]
                 [org.clojure/data.json "0.1.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [carneades-mapcomponent "1.0.0-SNAPSHOT"]]
  :dev-dependencies [[lein-ring "0.4.5"]
                     [swank-clojure "1.4.0-SNAPSHOT"]]
  :ring {:handler impact.web.routes/app})
