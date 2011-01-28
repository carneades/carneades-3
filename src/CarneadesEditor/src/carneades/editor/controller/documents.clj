;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.documents
  (:use clojure.pprint
        clojure.java.io
        (clojure.contrib def trace)
        (carneades.engine utils argument lkif)
        carneades.editor.utils.core
        (carneades.editor.model docmanager lkif-utils)
        carneades.editor.view.viewprotocol))

(defvar *docmanager* (create-docmanager))

;; TODO moves this info to the docmanager?
(defvar *fresh-ags-id* (atom {}))

(defn add-fresh-ag [path id]
  "adds the id of an argument graph to the map
   of newly created ags."
  (let [fresh (deref *fresh-ags-id*)]
    (if-let [ids (get fresh path)]
      (let [ids (conj ids id)]
        (swap! *fresh-ags-id* assoc path ids))
      (swap! *fresh-ags-id* assoc path #{id}))))

(defn remove-fresh-ags [path]
  (let [fresh (deref *fresh-ags-id*)]
    (swap! *fresh-ags-id* assoc path #{})))

(defn get-fresh-ag-ids [path]
  (get (deref *fresh-ags-id*) path #{}))

(defn update-dirty-state [view path id isdirty]
  (set-dirty view path id isdirty))

(defn update-undo-redo-statuses [view path id]
  (prn "update-undo-redo-statuses")
  (set-can-undo view path id (can-undo-section? *docmanager* [path :ags id]))
  (set-can-redo view path id (can-redo-section? *docmanager* [path :ags id])))

(defn ag-dirty? [path id]
  (section-dirty? *docmanager* [path :ags id]))

(defn get-ag [lkifpath id]
  (get-section-content *docmanager* [lkifpath :ags id]))

(defn get-lkif [lkifpath]
  (extract-lkif-from-docmanager lkifpath *docmanager*))

(defn get-rules [lkifpath]
  ;; to delete
  (:rules (:rb (get-lkif lkifpath))))

(defn get-reasoners [path]
  (keep :reasoner (vals (:import-kbs (get-lkif path)))))

;; (defn get-kbs-locations [lkifpath]
;;   (map first (get-section-content *docmanager* [lkifpath :import-kbs])))

(defn get-imports-locations [lkifpath]
  (prn "section content =")
  (prn (get-section-content *docmanager* [lkifpath :import-tree]))
  (map (fn [{:keys [name relative-path]}]
         (or relative-path name))
       (get-section-content *docmanager* [lkifpath :import-tree])))

(defn get-ags-id [lkifpath]
  (get-all-sectionskeys *docmanager* [lkifpath :ags]))

(defn get-allpaths []
  (filter string? (get-all-sectionskeys *docmanager* [])))

(defn get-unsaved-graphs 
  "returns a ([path id] [path id] ...) seq
   or (id1 id2 id3) when called with one argument "
  ([]
     (mapcat (fn [path]
               (partition 2 (interleave
                             (repeat path)
                             (filter #(ag-dirty? path %) (get-ags-id path)))))
             (get-allpaths)))
  ([path]
     (filter #(ag-dirty? path %) (get-ags-id path))))

(defvar- *non-ag-sections* (keys (dissoc *empty-lkif* :ags)))

(defn mark-lkif-saved [view path]
  (doseq [s *non-ag-sections*]
   (mark-section-saved *docmanager* [path s]))
  (doseq [id (get-ags-id path)]
    (let [ag (get-ag path id)]
      (mark-section-saved *docmanager* [path :ags id])
      (update-dirty-state view path id false)))
  (set-lkif-dirty view path false))

(deftrace lkif-dirty? [path]
  (or (some #(section-dirty? *docmanager* [path %]) *non-ag-sections*)
      (some #(section-dirty? *docmanager* [path :ags %]) (get-ags-id path))))

;; (defn get-unsaved-lkifs []
;;   (keep (fn [path]
;;           (when-not (empty? (get-unsaved-graphs path))
;;             path))
;;         (get-allpaths)))
(defn get-unsaved-lkifs []
  (filter lkif-dirty? (get-allpaths)))

(defn init-counters [path]
  (add-section *docmanager* [path :graph-counter] 1)
  (doseq [id (get-ags-id path)]
    (add-section *docmanager* [path id :stmt-counter] 1)))

(defn init-stmt-counter [path id]
  (add-section *docmanager* [path id :stmt-counter] 1))

(defn stmt-counter-value [path id]
  (let [v (get-section-content *docmanager* [path id :stmt-counter])]
    (update-section *docmanager* [path id :stmt-counter] (inc v))
    v))

(defn graph-counter-value [path]
  (let [v (get-section-content *docmanager* [path :graph-counter])]
    (update-section *docmanager* [path :graph-counter] (inc v))
    v))

(defn gen-statement-content [path ag]
  (let [stmt (str "statement_" (stmt-counter-value path (:id ag)))]
    (if (statement-node ag stmt)
      (gen-statement-content path ag)
      stmt)))

(defn gen-graph-title [path]
  (let [title (str "graph" (graph-counter-value path))
        titles (set (map :title (map #(get-ag path %) (get-ags-id path))))]
    (if (get titles title)
      (gen-graph-title path)
      title)))

(defn gen-argument-id [ag]
  (let [id (gensym "a")]
    (if (get-argument ag id)
      (gen-argument-id ag)
      id)))

(defn gen-graph-id [path]
  (let [id (gensym "ag")]
    (if (get-ag path id)
      (gen-graph-id path)
      id)))

(defvar *newlkif-prefix* "untitled-file")
(defvar *newlkif-suffix* ".xml")

(defn get-newlkif-filename [idx-or-path]
  (if (string? idx-or-path)
    (let [paths (get-section-content *docmanager* [:newlkif-paths])]
      (get-newlkif-filename (get paths idx-or-path)))
    (str *newlkif-prefix* idx-or-path *newlkif-suffix*)))

(defn gen-newlkif-filename [path]
  (if-let [idx (get-section-content *docmanager* [:newlkif-indexes])]
    (let [firstfree (first (filter #(not (contains? idx %)) (rest (range))))
          filename (get-newlkif-filename firstfree)
          newlkif-paths (get-section-content *docmanager* [:newlkif-paths])]
      (update-section *docmanager* [:newlkif-indexes] (conj idx firstfree))
      (update-section *docmanager* [:newlkif-paths] (assoc newlkif-paths path firstfree))
      filename)
    (let [filename (get-newlkif-filename 1)]
      (add-section *docmanager* [:newlkif-indexes] #{1})
      (add-section *docmanager* [:newlkif-paths] {path 1})
      filename)))

(defn new-lkif? [path]
  (when-let [paths (set (keys (get-section-content *docmanager* [:newlkif-paths])))]
    (contains? paths path)))

(defn remove-newlkif [path]
  (when-let* [paths (get-section-content *docmanager* [:newlkif-paths])
              idx (get-section-content *docmanager* [:newlkif-indexes])
              index (get paths path)
              paths (dissoc paths path)
              idx (disj idx index)]
    (prn "remove-newlkif, path =")
    (prn path)
    (prn "idx =")
    (prn idx)
    (prn "index =")
    (prn index)
    (update-section *docmanager* [:newlkif-indexes] idx)
    (update-section *docmanager* [:newlkif-paths] paths)))

(defn get-graphs-titles [path]
  "returns a set of all titles"
  (set (map :title (map #(get-ag path %) (get-ags-id path)))))

(defn do-ag-update [view keys ag]
  "updates section content in the model and dirty markers in the view"
  ;; the first key is the path
  (let [path (first keys)]
    (update-section *docmanager* keys ag)
    (update-undo-redo-statuses view path (:id ag))
    (update-dirty-state view path (:id ag) true)))

(defn update-imports [view path lkif]
  (prn "non-ag-sections = ")
  (prn *non-ag-sections*)
  (doseq [k *non-ag-sections*]
    (update-section *docmanager* [path k] (get lkif k))
    (delete-section-history *docmanager* [path k]))
  (set-lkif-dirty view path true))

(defn as-absolute-import [lkifpath importurl]
  (if (or (url? importurl) (.isAbsolute (file importurl)))
    importurl
    (:name
     (first (filter (fn [{:keys [relative-path]}]
                      (or (= relative-path importurl)))
                    (get-section-content *docmanager* [lkifpath :import-tree]))))))

