;; Copyright (c) 2010-2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Read and save properties from the properties file"}
  carneades.config.config
  (:use [carneades.engine.utils :only [exists?
                                       safe-read-string
                                       file-separator]]))

(def configfilename
  (let [default-pathname "config/carneades.clj"
        configuration-property (System/getProperty "carneades.configuration")]
    ;; if there is a java system properties 'carneades.configuration' then its value
    ;; is the path to the configuration file
    (cond configuration-property configuration-property
          (exists? default-pathname)
          ;; otherwise if there is property file in the current directory, we take it
          ;; otherwise we go for the one in the user's HOME directory
          default-pathname
          :else (str (System/getProperty "user.home")
                     file-separator
                     ".carneades.clj"))))

(defn read-properties
  "Reads the properties contained in pathname and returns a map."
  [pathname]
  (safe-read-string (slurp pathname)))

(defn save-properties
  "Saves the properties to a file."
  [props pathname]
  (spit (pr-str props) pathname))

(def properties
  (delay
   (try
     (read-properties configfilename)
     (catch Exception _
       (do
         (printf "The configuration file %s is missing or has invalid content."
                 configfilename)
         (throw (ex-info "Invalid or missing configuration file"
                         {:configfilename configfilename})))))))

(defn get-projects-directory
  "Returns the absolute pathname of the project's directory.
If the projects-directory key is not specified in the config file, the current
  'projects' directory is used ; this is useful when packaging Carneades a
  self-executable JAR."
  []
  (or (@properties :projects-directory)
      (str (System/getProperty "user.dir") file-separator "projects")))
