;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.handlers.messages
  (:use clojure.contrib.def))

(defvar *goalwizard-title* "Goal Assistant")
(defvar *goalwizard-error* "Goal Assistant Error")
(defvar *no-mainissue* "The main issue is not defined. Please assign a main issue to the graph first.")

(defvar *point-of-view* "Choose A Point Of View")
(defvar *choose-point-of-view* "Choose a point of view")
;; (defvar *positions* "Choose A Position")
;; (defvar *select-position* "Select a position")

(defvar *statements* "Choose A Statement")
(defvar *select-statement* "Select a statement")
(defvar *goal-achieved* "Your goal has already been achieved.")
(defvar *searching-positions* "Searching positions... Please wait...")