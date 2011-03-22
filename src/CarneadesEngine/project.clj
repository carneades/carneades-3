;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(defproject carneades-engine "1.0.0-SNAPSHOT"
  :description "Carneades is an argument mapping application, with a graphical 
user interface, and a software library for building applications supporting 
various argumentation tasks. This is the software library (the engine)."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.pallix/owlapi "3.0.0"]
                 [org.clojars.pallix/hermit "1.2.4"]]
  :dev-dependencies [[swank-clojure "1.2.0"]
                     [org.clojars.rayne/autodoc "0.8.0-SNAPSHOT"]])
