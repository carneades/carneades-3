;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.application.wizards.findarguments
  (:use clojure.contrib.def
        carneades.editor.view.wizardsprotocol)
  (:import (carneades.editor.uicomponents.wizards.findarguments SearchParametersPanel
                                                                SearchArgumentsPanel)))

(defvar- *searchParametersPanel* (SearchParametersPanel/instance))
(defvar- *goalTextArea* (.goalTextArea *searchParametersPanel*))

(defvar- *searchArgumentsPanel* (SearchArgumentsPanel/instance))
(defvar- *searchResultsPanel* (.searchResultsPanel *searchArgumentsPanel*))
(defvar- *cardLayout* (.getLayout *searchResultsPanel*))

(defvar- *dummyValidatorTrigger* (.dummyValidatorTrigger *searchArgumentsPanel*))

(deftype EditorSwingFindArgumentsWizard []
  SwingFindArgumentsWizard
  (set-goal
   [this text]
   (.setText *goalTextArea* text))

  (get-searchparameters-panel
   [this]
   *searchParametersPanel*)

  (get-searcharguments-panel
   [this]
   (.setVisible *dummyValidatorTrigger* false)
   *searchArgumentsPanel*)

  (set-argumentsearch-busy
   [this busy]
   (.setText *dummyValidatorTrigger* "xyz")
   (if busy
     (.show *cardLayout* *searchResultsPanel* "searchingArguments")
     (.show *cardLayout* *searchResultsPanel* "searchFinishedWithResults")))

  (arguments-found
   [this found]
   (if found
     (.show *cardLayout* *searchResultsPanel* "searchFinishedWithResults")
     (.show *cardLayout* *searchResultsPanel* "searchFinishedWithNoResult"))))
