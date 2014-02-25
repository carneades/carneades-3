(ns carneades.web.modules.lican.entity
  (:require [carneades.web.modules.lican.triplestore :as triplestore]
            [carneades.project.admin :as project]))

(defn get-software-entity
  "Returns information about the software entity"
  [project uri]
  (let [properties (project/load-project-properties project)
        triplestore (:triplestore properties)
        repo-name (:repo-name properties)
        markos-namespaces (:namespaces properties)]
    {:name (triplestore/get-entity-name triplestore repo-name markos-namespaces uri)}))
