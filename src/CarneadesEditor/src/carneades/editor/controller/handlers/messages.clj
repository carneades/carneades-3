;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.handlers.messages
  (:use clojure.contrib.def))


(defvar *file-error* "File Error")
(defvar *edit-error* "Edit Error")
(defvar *open-error* "Open Error")
(defvar *save-error* "Save Error")
(defvar *import-error* "Import Error")
(defvar *save-as* "Save As")
(defvar *statement-already-exists* "Statement already exists.")
(defvar *file-already-opened* "File %s is already opened.")
(defvar *file-format-not-supported* "This file format is not supported.")
(defvar *invalid-content* "The content of the file is invalid")
(defvar *error-saving* "Error while saving")
(defvar *config-error* "Configuration Error")

(defvar *cycle-error* "This premise can not be added as this would introduce a cycle.")

(defvar *goalwizard-title* "Goal Assistant")
(defvar *findargumentswizard-title* "Find Arguments")
(defvar *findargumentswizard-error* "Find Arguments Error")
(defvar *goalwizard-error* "Goal Assistant Error")
(defvar *no-mainissue* "Please assign a main issue to the graph first.")

(defvar *point-of-view* "Choose a Point Of View")
(defvar *choose-point-of-view* "Choose a point of view")
;; (defvar *positions* "Choose A Position")
;; (defvar *select-position* "Select a position")

(defvar *statements* "Choose A Statement")
(defvar *select-statement* "Select a statement")
(defvar *goal-achieved* "Your goal has already been achieved.")
(defvar *searching-positions* "Searching positions... Please wait...")
(defvar *invalid-content* "Content is invalid")
(defvar *cannot-open* "Argumentation Schemes File '%s' cannot be open")

(defvar *search-parameters* "Choose Search Parameters")
(defvar *no-statementselected* "Please select a statement first.")
(defvar *search-arguments* "Arguments Search")
(defvar *searching-arguments* "Searching arguments... Please wait...")

(defvar *imports* "Imports")
(defvar *remove-imports* "Permanently remove all selected imports?")

(defvar *instantiatescheme-title* "Instantiate Scheme")
(defvar *schemes-panel* "Choose a Scheme")
(defvar *select-a-scheme* "Select a Scheme")

(defvar *formalizestatement-title* "Formalize Statement")
(defvar *formalizestatementwizard-error* "Formalize Statement Assistant Error")
(defvar *entities-panel-desc* "Choose a Class or Property")
(defvar *select-an-entity* "Select an Entity")
(defvar *statement-panel-desc* "Complete the Formular")

(defvar *clauses-panel-desc* "Choose a Clause")
;; (defvar *literal-panel-desc* "Choose a Literal")
(defvar *complete-form-n* "Fill in Literal %s")
        
(defvar *fillin-form* "Fill in the form")

(defvar *rename* "Rename")
(defvar *warning-on-rename* "After renaming you will not be able to undo the last changes.\nRename the graph?")

(defvar *no-rule-directory* "Please specify a Rule Directory in the Preferences first.")
(defvar *cannot-be-relative* "%s must be under %s or %s to be relative.")