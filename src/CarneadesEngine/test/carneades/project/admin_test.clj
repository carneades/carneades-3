(ns carneades.project.admin-test
  (:use [clojure.test :only [deftest is]])
  (:require [carneades.project.admin :as project]))

(deftest relative-theory-path
  (is (= "copyright/theories/copyright_policies.clj"
         (project/relative-theory-path "copyright"
                                       "copyright/copyright_policies")))
  (is (= "copyright/theories/copyright_policies.clj"
         (project/relative-theory-path "copyright"
                                       "copyright_policies")))
  (is (= "default/theories/walton_scheme.clj"
         (project/relative-theory-path "copyright" "default/walton_scheme"))) )
