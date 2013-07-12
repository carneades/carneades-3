;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Management functions for the projects."}
  carneades.project.admin
  (:use [carneades.engine.utils :only [file-separator exists? file-separator]])
  (:require [carneades.config.config :as config]
            [carneades.engine.theory :as theory]
            [clojure.string :as s]))

(def projects-directory (config/properties :projects-directory
                                           (str (System/getProperty "user.dir")
                                                file-separator
                                                "projects")))

(def theories-directory "theories")

(defn- project?
  "Returns true if the director is a project."
  [dir]
  (and (.isDirectory dir)
       (exists? (str dir file-separator "properties.clj"))))

(defn list-projects
  "Returns a list of project names. A read on the disk is performed."
  []
  (let [dirs (into [] (.listFiles (clojure.java.io/file projects-directory)))
        projects (filter project? dirs)]
    (map (memfn getName) projects)))

(defn load-project-properties
  "Returns the project properties as a map."
  [project]
  (let [project-path (str projects-directory file-separator project)
        properties-path (str project-path file-separator "properties.clj")
        project-properties (config/read-properties properties-path)]
    project-properties))

(defn relative-theory-path
  "Returns the absolute path of a theory name. The name of the theory
can be of the form \"theory\" or \"project/theory\". The former refers
  to the current project, the latter to a the theory of project in the
  projects directory."
  [project theory]
  (let [[project-or-file file] (s/split theory #"/")]
    (if file
      (str project-or-file file-separator
           theories-directory file-separator
            file ".clj")
      (str project file-separator
           theories-directory file-separator
           project-or-file ".clj"))))

(defn absolute-theory-path
  "Returns the absolute path of a theory."
  [project theory]
  (str projects-directory file-separator
       (relative-theory-path project theory)))

(defn load-theory
  "Loads the theory of a project"
  [project theory]
  {:pre [(not (nil? project))]}
  (let [project-path (str projects-directory file-separator project)
        theory-path (absolute-theory-path project theory)]
    (theory/load-theory theory-path)))

(defn- load-policy
  "Loads the policy of a project"
  [project project-properties]
  (when-let [policy (:policies project-properties)]
    (load-theory project policy)))

(defn load-project
  "Loads the configuration of a project and its policy. Returns a map
representing the project."
  [project]
  (let [project-properties (load-project-properties project)
        policy-properties (:policies project-properties)
        policy (load-policy project project-properties)]
    {:properties project-properties}))
