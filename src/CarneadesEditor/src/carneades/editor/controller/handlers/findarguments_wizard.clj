;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.handlers.findarguments-wizard
  (:use clojure.contrib.def
        clojure.contrib.pprint
        carneades.editor.controller.documents
        carneades.editor.controller.handlers.messages
        (carneades.engine shell statement lkif)
        [carneades.engine.search :only (depth-first breadth-first)]
        (carneades.editor.view swinguiprotocol viewprotocol wizardsprotocol)))

(defn- get-selected-statement [view path id]
  (when-let [node (get-selected-node view path id)]
    (when (statement? node)
      node)))

(defvar- *goal* (atom nil))

(defn on-pre-findarguments-wizard [view path id]
  (when-let [ag (get-ag path id)]
    (let [goal (get-selected-statement view path id)]
      (if (nil? goal)
        (do
          (display-error view *findargumentswizard-error* *no-statementselected*)
          false)
        (do
          (reset! *goal* goal)
          (set-goal view (statement-formatted goal))
          true)))))

(defn on-post-findarguments-wizard [view path id settings]
  (prn "on-post-findarguments-wizard")
  (prn "settings = ")
  (prn settings)
  (when settings
   (when-let [ag (get-ag path id)]
     (let [complement (get settings "complement")
           goal (deref *goal*)
           goal (if complement (statement-complement goal) goal)
           lkif (get-lkif path)
           max-nodes (get settings "max-nodes")
           max-turns (get settings "max-turns")
           search-strategy (get settings "search-strategy")
           strategy (case search-strategy
                          "Depth first" depth-first
                          "Breadth first" breadth-first)
           solutions (construct-arguments goal max-nodes max-turns strategy ag
                                          (list (generate-arguments-from-lkif lkif)))
           ag2 (assoc (unite-solutions solutions)
                :id (:id ag)
                :main-issue (:main-issue ag)
                :title (:title ag))]
       ;; (prn "lkif = ")
       ;; (prn lkif)
       ;; (prn "solutions =")
       ;; (pprint solutions)
       ;; (prn "ag =")
       ;; (pprint ag2)
       ;; (prn)
       ;; (if (empty? (:arguments ag2))
       ;;   ag
       ;;   ag2)
       (do-update-section view [path :ags (:id ag)] ag2)
       (graph-changed view path ag2 statement-formatted)
       (display-statement view path ag2 goal statement-formatted)
       ))))