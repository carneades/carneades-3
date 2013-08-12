;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Management functions for the projects."}
  carneades.project.admin
  (:use [carneades.engine.utils :only [file-separator exists? file-separator make-relative]])
  (:require [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [me.raynes.fs :as fs]
            [carneades.config.config :as config]
            [carneades.engine.theory :as theory]))

(def projects-directory (config/properties :projects-directory
                                           (str (System/getProperty "user.dir")
                                                file-separator
                                                "projects")))

(def theories-directory "theories")

(def documents-directory "documents")

(def projects-lock (Object.))

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

(defn- get-properties-path
  "Returns the path of the project's properties"
  [project]
  (let [project-path (str projects-directory file-separator project)
        properties-path (str project-path file-separator "properties.clj")]
    properties-path))

(defn- load-project-properties
  "Returns the project properties as a map."
  [project]
  (let [properties-path (get-properties-path project)
        project-properties (config/read-properties properties-path)]
    project-properties))

(defn relative-theory-path
  "Returns the relative path of a theory name. The name of the theory
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

(defn list-theories-files
  "Returns a list of available theories files for the project."
  [project]
  (let [project-path (str projects-directory file-separator project)
        theories-dir (str project-path file-separator theories-directory)
        files (fs/find-files theories-dir #".*\.clj")
        names (map (memfn getName) files)]
    names))

(defn list-documents
  "Returns a list of the documents names"
  [project]
  (fs/list-dir (str projects-directory file-separator project file-separator documents-directory)))

(defn load-project
  "Loads the configuration of a project and its policy. Returns a map
representing the project."
  [project]
  (locking projects-lock
    (let [remove-extension (fn [f] (subs f 0 (- (count f) 4)))
          project-properties (load-project-properties project)
          policy-properties (:policies project-properties)
          policy (load-policy project project-properties)
          theories-files (list-theories-files project)
          theories (map remove-extension theories-files)
          documents (list-documents project)]
      {:properties project-properties
       :available-theories theories
       :documents documents})))

(defn delete-project
  "Permanently delete project from the disk."
  [project]
  (fs/delete-dir (str projects-directory file-separator project)))

(defn update-project-properties
  "Update the properties file of the project."
  [project properties]
  {:pre [(map? properties)
         (not (:id properties))
         (not (some #(and (string? %) (empty? %)) (vals properties)))]}
  (locking projects-lock
    (let [old-properties (load-project-properties project)
          new-properties (merge old-properties properties)
          properties-path (get-properties-path project)]
      (spit properties-path (pp/write new-properties :stream nil)))))

(defn import-theories
  "Imports the theories stored at pathname into project as name."
  [project pathname name]
  (let [dest (str projects-directory file-separator
                  project file-separator
                  theories-directory file-separator
                  name)]
    (fs/copy pathname dest)))

(defn delete-theories
  "Delete the theories of the project."
  [project theories]
  (let [pathname (str projects-directory file-separator
                  project file-separator
                  theories-directory file-separator
                  (str theories ".clj"))]
    (fs/delete pathname)))

(defn document-path
  "Returns the path of a project's document."
  [project document]
  (str projects-directory file-separator
       project file-separator
       documents-directory file-separator
       document))

(defn delete-document
  "Deletes the document of the project."
  [project document]
  (let [pathname (document-path project document)]
    (fs/delete pathname)))

(defn import-document
  "Imports a document into the project."
  [project pathname name]
  (let [dest (document-path project name)]
    (fs/copy pathname dest)))

(defn create-project
  "Creates a new project in the projects' directory"
  [project]
  (let [docpath (str projects-directory file-separator
                     project file-separator
                     documents-directory)
        theoriespath (str projects-directory file-separator
                          project file-separator
                          theories-directory)]
    (fs/mkdirs docpath)
    (fs/mkdir theoriespath)))

(defn delete-project
  "Delete project from project's directory."
  [project]
  (let [path (str projects-directory file-separator project)]
    (fs/delete-dir path)))
