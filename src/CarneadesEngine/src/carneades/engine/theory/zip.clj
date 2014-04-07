;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Zipper for a theory. @See clojure.zip"}
  carneades.engine.theory.zip
  (:require [clojure.zip :as z]))

(defn- may-have-children?
  [node]
  (contains? node :sections))

(defn- make-node
  [node children]
  (assoc node :sections (into [] children)))

(defn theory-zip
  "Returns a zipper for a theory. The zipper navigates
through the sections of the theory."
  [theory]
  (z/zipper may-have-children?
            (comp seq :sections)
            make-node
            theory))
