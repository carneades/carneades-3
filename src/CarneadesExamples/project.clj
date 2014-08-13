;; Copyright (c) 2012 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defproject carneades-examples/carneades-examples "2.0.0-SNAPSHOT"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [carneades/carneades-engine "2.0.3"]
                 ]
  :description "Carneades is an argument mapping application, with a graphical \nuser interface, and a software library for building applications supporting \nvarious argumentation tasks. These are some examples."
  :cljsbuild {:builds []})
