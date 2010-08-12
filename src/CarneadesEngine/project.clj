;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(defproject carneades-engine "1.0.0-SNAPSHOT"
  :description "Carneades is an argument mapping application, with a graphical 
user interface, and a software library for building applications supporting 
various argumentation tasks. This is the software library (the engine)."
  :dependencies [[org.clojure/clojure "1.2.0-RC1"]
                 [org.clojure/clojure-contrib "1.2.0-RC1"]
                 [org.clojars.pallix/owlapi "3.0.0"]
				 [org.clojars.pallix/pellet-core "2.1.1"]
				 [org.clojars.pallix/pellet-owlapiv3 "2.1.1"]
				 [org.clojars.pallix/pellet-rules "2.1.1"]
				 [org.clojars.pallix/pellet-datatypes "2.1.1"]
				 [org.clojars.pallix/pellet-el "2.1.1"]
				 [org.clojars.pallix/aterm-java "1.6"]
                 [clj-sandbox "0.3.8"]]
  :dev-dependencies [[swank-clojure "1.2.0"]
                     [lein-run "1.0.0-SNAPSHOT"]])
