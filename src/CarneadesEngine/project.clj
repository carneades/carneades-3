(defproject carneades-engine/carneades-engine "2.0.0-SNAPSHOT" 
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "[1.4.0,]"]
                 [org.clojars.pallix/owlapi "3.0.0"]
                 [org.clojars.pallix/hermit "1.2.4"]
                 [org.clojure/math.combinatorics "0.0.2"]
                 [weissjeffm/clojure.prxml "1.3.0-SNAPSHOT"]
                 [com.h2database/h2 "1.3.160"]
                 [org.clojure/java.jdbc "0.1.1"]
                 [org.clojure/tools.trace "0.7.1"]
                 [lacij "0.7.1"]]
  :aot [carneades.engine.argument-evaluation]
  :description "Carneades is an argument mapping application, with a graphical \nuser interface, and a software library for building applications supporting \nvarious argumentation tasks. This is the software library (the engine)."
  :cljsbuild {:builds []})
