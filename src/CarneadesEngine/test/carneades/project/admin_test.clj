(ns carneades.project.admin-test
  (:use [clojure.test :only [deftest is use-fixtures]]
        [carneades.engine.uuid :only [make-uuid-str]]
        [carneades.engine.utils :only [file-separator]])
  (:require [carneades.project.admin :as project]
            [me.raynes.fs :as fs]))

(def project-name (str "testproject-" (make-uuid-str)))

(defn create-tmp-project
  []
  (project/create-project project-name))

(defn delete-tmp-project
  []
  (project/delete-project project-name))

(defn db-fixture [x]
  (create-tmp-project)
  (x)
  (delete-tmp-project))

(use-fixtures :once db-fixture)


(deftest relative-theory-path
  (is (= "copyright/theories/copyright_policies.clj"
         (project/relative-theory-path "copyright"
                                       "copyright/copyright_policies")))
  (is (= "copyright/theories/copyright_policies.clj"
         (project/relative-theory-path "copyright"
                                       "copyright_policies")))
  (is (= "default/theories/walton_scheme.clj"
         (project/relative-theory-path "copyright" "default/walton_scheme"))) )

(deftest import-document
  (let [docfile (fs/temp-file "doc-")]
    (spit docfile (str "content-" (make-uuid-str)))
    (project/import-document project-name (.getPath docfile) (.getName docfile))
    (is (fs/exists? (str project/projects-directory file-separator
                         project-name file-separator
                         project/documents-directory file-separator
                         (.getName docfile))))))
