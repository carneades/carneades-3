;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.application.wizards.instantiatescheme
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.view.application.wizards.messages
        carneades.editor.view.wizardsprotocol)
  (:require [clojure.string :as str])
  (:import (java.awt.event KeyAdapter)
           (javax.swing JPanel JTextField JLabel BoxLayout SpringLayout)
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
   [this formid literal suggestions variables suffix listener args]
   (let [panel (LiteralPanel.)
         literalText (.literalText panel)
         variablesPanel (.variablesPanel panel)
         textfields (mapcat (fn [var]
                              (let [varpanel (VariablePanel.)
                                    label (.variableLabel varpanel)
                                    text (.variableText varpanel)]
                                (.setText label (str var))
                                (.setName text (str var "-" suffix))
                                (.add variablesPanel varpanel)
                                (.addKeyListener text
                                                 (proxy [KeyAdapter] []
                                                   (keyReleased
                                                    [keyevent]
                                                    (apply listener formid var (.getText text) args))))
                                [var text]))
                         variables)]
     (.setText literalText literal)
     (LiteralFormular. formid panel (apply hash-map textfields)))
   )

  (fillin-formular
   [this formular var-values]
   (let [textfields (:textfields formular)]
     (doseq [[var value] var-values]
       (when-let [text (get textfields var)]
         (.setText text value)))))

  (set-filter-text-listener
   [this f args]
   (doseq [l (.getKeyListeners *filterText*)]
     (.removeKeyListener *filterText* l))
   (.addKeyListener *filterText*
                    (proxy [KeyAdapter] []
                      (keyReleased
                       [keyevent]
                       (apply f keyevent args))))))
