;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Helper functions to take the content of a LKIF structure and put
            it in the document manager."}
  carneades.editor.model.lkif-utils
  (:use clojure.contrib.def
        clojure.contrib.pprint
        carneades.engine.rule
        carneades.editor.model.docmanager))

(defn- dissect [lkifcontent]
  "takes the content of an LKIF representation and returns a sequence of [keys sectioncontent]
   suitable for the docmanager. Keys is a vector"
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
    (let [keys (concat [lkifpath] keys)]
     (add-section docmanager keys section)
     (mark-section-saved docmanager keys))))

(defn extract-lkif-from-docmanager [lkifpath docmanager]
  (let [rb (get-section-content docmanager [lkifpath :rb])
        sources (get-section-first-content docmanager [lkifpath :sources])
        agids (get-all-sectionskeys docmanager [lkifpath :ags])
        ags (doall (map #(get-section-content docmanager [lkifpath :ags %]) agids))
        import-tree (get-section-content docmanager [lkifpath :import-tree])
        import-kbs (get-section-content docmanager [lkifpath :import-kbs])
        import-ags (get-section-content docmanager [lkifpath :import-ags])]
    {:rb rb :sources sources :ags ags
     :import-tree import-tree
     :import-kbs import-kbs
     :import-ags import-ags}))


