;; Copyright (c) 2012 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defproject carneades/carneades-engine "2.0.3"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojars.pallix/owlapi "3.4.5"]
                 [org.clojars.pallix/hermit "1.2.4"]
                 [org.clojars.pallix/clojure.prxml "1.3.0"]
                 [org.clojure/math.combinatorics "0.0.2"]
                 [com.h2database/h2 "1.3.160"]
                 [org.clojure/java.jdbc "0.3.3"]
                 [org.clojure/tools.trace "0.7.1"]
                 [edu.ucdenver.ccp/kr-sesame-core "1.4.5"]
                 [lacij "0.9.0"]
                 [me.raynes/fs "1.4.0"]
                 [com.taoensso/timbre "3.1.6"]
                 [org.slf4j/slf4j-log4j12 "1.7.5"]
                 [org.clojure/tools.namespace "0.2.4"]
                 [midje "1.5.1"]]
  :plugins [[lein-midje "3.0.0"]]
  :aot [carneades.engine.argument-evaluation]
  :description "Carneades is an argument mapping application, with a graphical \nuser interface, and a software library for building applications supporting \nvarious argumentation tasks. This is the software library (the engine)."
  :cljsbuild {:builds []})
