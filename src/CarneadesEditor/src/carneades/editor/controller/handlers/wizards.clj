;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.handlers.wizards
  (:use carneades.editor.view.viewprotocol
        carneades.editor.view.wizardsprotocol
        carneades.editor.controller.handlers.messages
        carneades.editor.controller.documents
        [carneades.engine.statement :only (statement-formatted)]
        [carneades.engine.abduction :only (statement-in-label
                                           statement-out-label
                                           assume-decided-statements)]))

(defn on-pre-goalwizard [view path id]
  (when-let [ag (get-ag path id)]
    (let [mainissue (statement-formatted (:main-issue ag))]
      (if (empty? mainissue)
        (do
          (display-error view *goalwizard-error* *no-mainissue*)
          false)
        (do
          (set-main-issue view mainissue)
          true)))))

(defn on-proponent-panel-validation [view path id settings]
  (let [proponent (get settings "proponent")
        opponent (get settings "opponent")]
    (if (and (not opponent) (not proponent))
      *choose-point-of-view*
      nil)))

(defn on-abduction-panel-validation [view path id settings]
  (let [position (get settings "positions")]
    (if (nil? position)
      *select-position*
      nil)))

(defn on-abduction-panel [view path id settings]
  (prn "on-abduction-panel")
  (prn settings)
  (when-let [ag (get-ag path id)]
    (let [proponent (get settings "proponent")
          mainissue (:main-issue ag)
          positions (if proponent
                      (statement-in-label ag (assume-decided-statements ag) mainissue)
                      (statement-out-label ag (assume-decided-statements ag) mainissue))]
      (display-abduction-result view positions statement-formatted))))

(defn on-statements-panel-validation [view path id settings]
  (let [statement (get settings "statements")]
    (if (nil? statement)
      *select-statement*
      nil)))

(defn on-statements-panel [view path id settings]
  (let [position (get settings "positions")]
    (prn "position =")
    (prn position)
    (prn "calling display statements")
    ;; todo: extract properly the statements from the position
    (display-statements view [position] statement-formatted)))

(defn on-post-goalwizard [view path id statement]
  (when-let [ag (get-ag path id)]
    (display-statement view path ag statement statement-formatted)))

