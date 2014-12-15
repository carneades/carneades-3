;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.project.fs-test
  (:require [carneades.project.fs :as project]
            [me.raynes.fs :as fs]
            [carneades.engine.uuid :refer [make-uuid-str]]
            [carneades.engine.utils :refer [file-separator]]
            [midje.sweet :refer :all]
            [carneades.config.config :as config]))

(def state (atom nil))

(defn initial-state-value
  []
  {:project-name (str "testproject-" (make-uuid-str))})

(defn create-tmp-project
  []
  (reset! state (initial-state-value))
  (project/create-project-files (:project-name @state) {}))

(defn delete-tmp-project
  []
  (project/delete-project (:project-name @state)))

;; failing. Should be fixed as part of issue #122
(with-state-changes [(before :facts (create-tmp-project))
                     (after :facts (delete-tmp-project))]
  (fact "The relative-path functions works."
        (expect (project/relative-theory-path "copyright" "copyright/copyright_policies")
                => "copyright/theories/copyright_policies.clj")
        (expect (project/relative-theory-path "copyright"
                                               "copyright_policies")
                 => "copyright/theories/copyright_policies.clj")
        (expect (project/relative-theory-path "copyright" "default/walton_scheme")
                => "default/theories/walton_scheme.clj"))
  (fact "Importing a document works"
        (let [docfile (fs/temp-file "doc-")
              project-name (:project-name @state)]
          (spit docfile (str "content-" (make-uuid-str)))
          (project/import-document project-name (.getPath docfile) (.getName docfile))
          (expect (fs/exists? (str (:projects-directory config/properties) file-separator
                               project-name file-separator
                               project/documents-directory file-separator
                               (.getName docfile)))
                  => true))))
