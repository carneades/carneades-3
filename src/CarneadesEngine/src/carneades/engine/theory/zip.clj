;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

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
