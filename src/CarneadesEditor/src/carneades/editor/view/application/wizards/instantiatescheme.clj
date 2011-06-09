;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Implementation of the view of the Instantiate Scheme assistant."}
  carneades.editor.view.application.wizards.instantiatescheme
  (:use clojure.contrib.def
        clojure.contrib.pprint
        clojure.contrib.swing-utils
        carneades.editor.utils.swing
        carneades.editor.view.application.wizards.messages
        carneades.editor.view.application.wizards.formular-utils
        carneades.editor.view.wizardsprotocol)
  (:require [clojure.string :as str])
  (:import (javax.swing JPanel JTextField JLabel BoxLayout SpringLayout)
           java.awt.FlowLayout
           (carneades.editor.view.wizardsprotocol StatementItem LiteralFormular)
           (carneades.editor.uicomponents.wizards.instantiatescheme
            SchemesPanel ClausesPanel LiteralPanel VariablePanel)))

(defvar- *schemesPanel* (SchemesPanel.))
(defvar- *schemesList* (.schemesList *schemesPanel*))
(defvar- *filterText* (.filterText *schemesPanel*))
(defvar- *conclusionText* (.conclusionText *schemesPanel*))
(defvar- *conclusionmatchesButton* (.conclusionmatchesButton *schemesPanel*))

(defvar- *clausesPanel* (ClausesPanel.))
(defvar- *clauseList* (.clauseList *clausesPanel*))
(defvar- *clauseLabel* (.clauseLabel *clausesPanel*))
(defvar- *previousClauseButton* (.previousClauseButton *clausesPanel*))
(defvar- *nextClauseButton* (.nextClauseButton *clausesPanel*))

(defvar- *clauseNumberText* (.clauseNumberText *clausesPanel*))

(deftype EditorInstantiateSchemeWizard []
  SwingInstantiateSchemeWizard

  (set-conclusion-statement
   [this stmt]
   (.setText *conclusionText* stmt))
  
  (get-schemes-panel
   [this]
   (.setSelected *conclusionmatchesButton* false)
   *schemesPanel*)

  (get-clauses-panel
   [this]
   (.setVisible *clauseNumberText* false)
   *clausesPanel*)

  (display-schemes
   [this descs]
   (.setListData *schemesList* (to-array descs)))

  (get-filter-text
   [this]
   (.getText *filterText*))

  (set-conclusionmatches-button-enabled
   [this enabled]
   (.setEnabled *conclusionmatchesButton* enabled))

  (conclusionmatches-button-selected?
   [this]
   (.isSelected *conclusionmatchesButton*))

  (conclusionmatches-button-enabled?
   [this]
   (.isEnabled *conclusionmatchesButton*))
  
  (set-conclusionmatches-button-listener
   [this f args]
   (remove-action-listeners *conclusionmatchesButton*)
   (apply add-action-listener *conclusionmatchesButton* f args))

  (display-clause
   [this clause clauseidx nb-clauses statement-formatted]
   (.setText *clauseNumberText* (str clauseidx))
   (let [items (to-array (map #(StatementItem.
                                  %
                                  (str "<html>"
                                       (str/replace (statement-formatted %) "\n" "<br>")
                                       "<br>&nbsp"))
                              clause))]
     (.setListData *clauseList* items)

     (.setText *clauseLabel* (format *clause-n-of* (inc clauseidx) nb-clauses))))

  (set-previous-clause-button-listener
   [this f args]
   (apply add-action-listener *previousClauseButton* f args))
  
  (set-next-clause-button-listener
   [this f args]
   (apply add-action-listener *nextClauseButton* f args))

  (create-literal-formular
   [this formid literal suggestions variables suffix listeners args]
   (create-formular formid *literal* literal suggestions variables suffix listeners args))

  (display-suggestion
   [this formular suggestion n nb-suggestions]
   (let [panel (:panel formular)
         suggestionText (.suggestionText panel)
         suggestionLabel (.suggestionLabel panel)
         previousSuggestionButton (.previousSuggestionButton panel)
         nextSuggestionButton (.nextSuggestionButton panel)
         useSuggestionButton (.useSuggestionButton panel)]
     (.setEnabled suggestionLabel true)
     (.setEnabled suggestionText true)
     (.setEnabled previousSuggestionButton true)
     (.setEnabled nextSuggestionButton true)
     (.setEnabled useSuggestionButton true)
     (.setText suggestionText suggestion)
     (.setText suggestionLabel (format *suggestion-n-of* n nb-suggestions))))

  (display-no-suggestion
   [this form]
   (let [panel (:panel form)
         suggestionText (.suggestionText panel)
         suggestionLabel (.suggestionLabel panel)
         previousSuggestionButton (.previousSuggestionButton panel)
         nextSuggestionButton (.nextSuggestionButton panel)
         useSuggestionButton (.useSuggestionButton panel)]
     (.setEnabled suggestionLabel false)
     (.setEnabled suggestionText false)
     (.setEnabled previousSuggestionButton false)
     (.setEnabled nextSuggestionButton false)
     (.setEnabled useSuggestionButton false)))

  (fillin-formular
   [this formular var-values]
   (fill-formular formular var-values))

  (trigger-formpanel-validator
   [this formular]
   (let [panel (:panel formular)
         trigger (.dummyValidatorTrigger panel)]
     (.setText trigger "xyz")))

  (set-filter-text-listener
   [this f args]
   (remove-key-listeners *filterText*)
   (apply add-key-released-listener *filterText* f args)))
