;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.handlers.findarguments-wizard
  (:use clojure.contrib.def
        carneades.engine.statement
        carneades.editor.controller.documents
        carneades.editor.controller.handlers.messages
        carneades.editor.view.swinguiprotocol
        carneades.editor.view.viewprotocol
        carneades.editor.view.wizardsprotocol))

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
