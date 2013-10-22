(ns carneades.project.admin-test
  (:require [carneades.project.admin :as project]
            [me.raynes.fs :as fs]
            [carneades.engine.uuid :refer [make-uuid-str]]
            [carneades.engine.utils :refer [file-separator]]
            [midje.sweet :refer :all]))

(def state (atom nil))

(defn initial-state-value
  []
  {:project-name (str "testproject-" (make-uuid-str))})

(defn create-tmp-project
  []
  (reset! state (initial-state-value))
  (project/create-project (:project-name @state)))

(defn delete-tmp-project
  []
  (project/delete-project (:project-name @state)))

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
          (expect (fs/exists? (str project/projects-directory file-separator
                               project-name file-separator
                               project/documents-directory file-separator
                               (.getName docfile)))
                  => true))))
