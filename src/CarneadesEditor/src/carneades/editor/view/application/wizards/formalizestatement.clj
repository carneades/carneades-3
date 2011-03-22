;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.application.wizards.formalizestatement
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.utils.swing
        carneades.editor.view.wizardsprotocol
        carneades.editor.view.application.wizards.formular-utils
        carneades.editor.view.application.wizards.messages)
  (:import (carneades.editor.uicomponents.wizards.formalizestatement
            EntitiesPanel StatementPanel)))

(defvar- *entitiesPanel* (EntitiesPanel.))
(defvar- *statementPanel* (StatementPanel. ))
(defvar- *literalPanel* (.literalPanel *statementPanel*))
(defvar- *statementTextArea* (.statementTextArea *statementPanel*))

(defvar- *entitiesList* (.entitiesList *entitiesPanel*))
(defvar- *classesButton* (.classesButton *entitiesPanel*))
(defvar- *propertiesButton* (.propertiesButton *entitiesPanel*))
(defvar- *filterText* (.filterText *entitiesPanel*))

(deftype EditorFormalizeStatementWizard []
  SwingFormalizeStatementWizard

  (get-entitiespanel
   [this]
   (.setText *filterText* "")
   (.setSelected *classesButton* true)
   (.setSelected *propertiesButton* true)
   *entitiesPanel*)

  (get-statement-panel
   [this statement listeners]
   (let [literal-formular
         (create-formular (gensym "form")
                          *predicate*
                          "" ()
                          '[?x ?y] "" listeners [])] 
     (.removeAll *literalPanel*)
     (.add *literalPanel* (:panel literal-formular))
     (.repaint *literalPanel*)
     [*statementPanel* literal-formular]))
  
  (set-filterentities-text-listener
   [this f args]
   (remove-key-listeners *filterText*)
   (apply add-key-released-listener *filterText* f args))
  
  (set-classes-button-listener
   [this f args]
   (remove-action-listeners *classesButton*)
   (apply add-action-listener *classesButton* f args))
  
  (set-properties-button-listener
   [this f args]
   (remove-action-listeners *propertiesButton*)
   (apply add-action-listener *propertiesButton* f args))

  (display-entities
   [this descs]
   (.setListData *entitiesList* (to-array descs)))

  (classes-button-selected?
   [this]
   (.isSelected *classesButton*))

  (properties-button-selected?
   [this]
   (.isSelected *propertiesButton*))

  (get-entities-filter-text
   [this]
   (.getText *filterText*))

  (prepare-statement-formular
   [this formular stmt entity]
   (.setText *statementTextArea* stmt)
   (set-literal-text formular (:formatted entity))
   (fill-formular formular '[[?x ""] [?y ""]])
   (set-field-visible formular '?y (= (:type entity) :property))))