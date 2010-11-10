;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.application.wizards.findarguments
  (:use clojure.contrib.def
        carneades.editor.view.wizardsprotocol)
  (:import (carneades.editor.uicomponents.wizards.findarguments SearchParametersPanel)))

(defvar- *searchParametersPanel* (SearchParametersPanel/instance))
(defvar- *goalTextArea* (.goalTextArea *searchParametersPanel*))

(deftype EditorSwingFindArgumentsWizard []
  SwingFindArgumentsWizard
  (set-goal
   [this text]
   (.setText *goalTextArea* text))

  (get-searchparameters-panel
   [this]
   *searchParametersPanel*))
