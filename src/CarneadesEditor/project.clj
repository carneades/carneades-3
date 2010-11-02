;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(defproject carneades-editor "0.9.0-SNAPSHOT"
  :description "Carneades is an argument mapping application, with a graphical user interface, and a software library for building applications supporting various argumentation tasks. This is the graphical editor."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [carneades-engine "1.0.0-SNAPSHOT"]
                 [carneades-editor-uicomponents "1.0.0-SNAPSHOT"]
                 [carneades-mapcomponent "1.0.0-SNAPSHOT"]
                 [org.clojars.pallix/wizard "0.9.0-SNAPSHOT"]]
  :dev-dependencies [[swank-clojure "1.2.0"]
                     [lein-run "1.0.0-SNAPSHOT"]]
  ;; uncomment the following line to create a self-executing JAR
  ;; and then invoke "cake uberjar" and pray.
  ;; "lein uberjar" does not work because of a bug.
  ;; :main carneades.editor.core
  )
