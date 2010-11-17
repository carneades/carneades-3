;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.handlers.messages
  (:use clojure.contrib.def))


(defvar *file-error* "File Error")
(defvar *edit-error* "Edit Error")
(defvar *open-error* "Open Error")
(defvar *save-error* "Save Error")
(defvar *save-as* "Save As")
(defvar *statement-already-exists* "Statement already exists.")
(defvar *file-already-opened* "File %s is already opened.")
(defvar *file-format-not-supported* "This file format is not supported.")
(defvar *invalid-content* "The content of the file is invalid")
(defvar *error-saving* "Error while saving")

(defvar *cycle-error* "This premise can not be added as this would introduce a cycle.")

(defvar *goalwizard-title* "Goal Assistant")
(defvar *findargumentswizard-title* "Find Arguments")
(defvar *findargumentswizard-error* "Find Arguments Error")
(defvar *goalwizard-error* "Goal Assistant Error")
(defvar *no-mainissue* "Please assign a main issue to the graph first.")

(defvar *point-of-view* "Choose A Point Of View")
(defvar *choose-point-of-view* "Choose a point of view")
;; (defvar *positions* "Choose A Position")
;; (defvar *select-position* "Select a position")

(defvar *statements* "Choose A Statement")
(defvar *select-statement* "Select a statement")
(defvar *goal-achieved* "Your goal has already been achieved.")
(defvar *searching-positions* "Searching positions... Please wait...")
(defvar *invalid-content* "Content is invalid.")

(defvar *search-parameters* "Choose Search Parameters")
(defvar *no-statementselected* "Please select a statement first.")
(defvar *search-arguments* "Arguments Search")
(defvar *searching-arguments* "Searching arguments... Please wait...")

(defvar *imports* "Imports")
(defvar *remove-imports* "Permanently remove all selected imports?")