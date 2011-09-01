(defproject impact-webroot "0.1.0"
  :description "Example Compojure project"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [compojure "0.6.4"]
                 [hiccup "0.3.6"]]
  :dev-dependencies [[lein-ring "0.4.5"]
                     [swank-clojure "1.4.0-SNAPSHOT"]]
  :ring {:handler impact.webroot.routes/app})
