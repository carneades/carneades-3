;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.model.lkif-utils
  (:use clojure.contrib.def
        carneades.engine.rule
        carneades.editor.model.docmanager))

(defn- dissect [lkifcontent]
  "takes the content of an LKIF representation and returns a sequence of [keys sectioncontent]
   suitable for the docmanager. Keys is a vector"
  (let [sources (:sources lkifcontent)
        rb (:rb lkifcontent)
        ags (:ags lkifcontent)]
    (concat (list [[:sources] sources] [[:rb] rb]) 
            (map (fn [ag] [[:ags (:id ag)] ag]) ags))))

(defn add-lkif-to-docmanager [lkifpath lkifcontent docmanager]
  (doseq [[keys section] (dissect lkifcontent)]
    (add-section docmanager (concat [lkifpath] keys) section)))

(defn extract-lkif-from-docmanager [lkifpath docmanager]
  (prn "get all sections keys =")
  (let [rb (get-section-first-content docmanager [lkifpath :rb])
        sources (get-section-first-content docmanager [lkifpath :sources])
        agids (get-all-sectionskeys docmanager [lkifpath :ags])
        ags (map #(get-section-first-content docmanager [lkifpath :ags %]) agids)]
    ;; (prn "rb =>")
    ;; (prn rb)
    ;; (prn "sources =>")
    ;; (prn sources)
    ;; (prn "ags =>")
    ;; (prn ags)
    {:rb rb :sources sources :ags ags}))

(defvar *empty-lkif-content*
  {:sources nil :rb *empty-rulebase* :ags ()})
