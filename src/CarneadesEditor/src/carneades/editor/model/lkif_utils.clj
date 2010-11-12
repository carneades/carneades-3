;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.model.lkif-utils
  (:use clojure.contrib.def
        clojure.contrib.pprint
        carneades.engine.rule
        carneades.editor.model.docmanager))

(defn- dissect [lkifcontent]
  "takes the content of an LKIF representation and returns a sequence of [keys sectioncontent]
   suitable for the docmanager. Keys is a vector"
  (prn "dissect")
  ;; (pprint lkifcontent)
  (let [sources (:sources lkifcontent)
        rb (:rb lkifcontent)
        ags (:ags lkifcontent)
        import-tree (:import-tree lkifcontent)
        import-kbs (:import-kbs lkifcontent)
        import-ags (:import-ags lkifcontent)]
    (concat (list [[:sources] sources] [[:rb] rb] [[:import-tree] import-tree]
                  [[:import-kbs] import-kbs] [[:import-ags] import-ags]) 
            (map (fn [ag] [[:ags (:id ag)] ag]) ags))))

(defn add-lkif-to-docmanager [lkifpath lkifcontent docmanager]
  (doseq [[keys section] (dissect lkifcontent)]
    (add-section docmanager (concat [lkifpath] keys) section)))

(defn extract-lkif-from-docmanager [lkifpath docmanager]
  (prn "get all sections keys =")
  (let [rb (get-section-first-content docmanager [lkifpath :rb])
        sources (get-section-first-content docmanager [lkifpath :sources])
        agids (get-all-sectionskeys docmanager [lkifpath :ags])
        ags (map #(get-section-first-content docmanager [lkifpath :ags %]) agids)
        import-tree (get-section-first-content docmanager [lkifpath :import-tree])
        import-kbs (get-section-first-content docmanager [lkifpath :import-kbs])
        import-ags (get-section-first-content docmanager [lkifpath :import-ags])]
    {:rb rb :sources sources :ags ags
     :import-tree import-tree
     :import-kbs import-kbs
     :import-ags import-ags}))

(defvar *empty-lkif-content*
  {:sources nil :rb *empty-rulebase* :ags ()})
