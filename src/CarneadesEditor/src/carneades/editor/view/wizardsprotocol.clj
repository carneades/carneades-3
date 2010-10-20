;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.wizardsprotocol)

(defprotocol SwingGoalWizard
  (set-main-issue [this mainissue])
  (get-proponent-panel [this])
  (get-abduction-panel [this])
  (get-positions-panel [this])
  (get-statements-panel [this])
  (display-abduction-result [this positions statement-formatted])
  (display-statements [this statements statement-formatted]))
