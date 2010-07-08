(defproject carneades-engine "1.0.0-SNAPSHOT"
  :description "Carneades is an argument mapping application, with a graphical 
user interface, and a software library for building applications supporting 
various argumentation tasks. This is the software library (the engine)."
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.2.0-master-SNAPSHOT"]
                 [org.clojars.pallix/owlapi "3.0.0"]
                 [clj-sandbox "0.3.8"]]
  :dev-dependencies [[swank-clojure "1.2.0"]
                     [lein-run "1.0.0-SNAPSHOT"]])
