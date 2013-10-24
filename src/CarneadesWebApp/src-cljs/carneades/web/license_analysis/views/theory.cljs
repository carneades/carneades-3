(ns carneades.web.license-analysis.views.theory
  (:require [carneades.analysis.web.views.header :as header]
            [carneades.web.license-analysis.views.menu :refer [lic-menu]]))


(defn ^:export show
  [project]
  (js/PM.load_project project)
  (header/show {:text :home
                :link "#/home"}
               (lic-menu project js/PM.entity))
  (js/PM.display_theory project))
