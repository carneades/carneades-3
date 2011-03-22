;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.application.wizards.formular-utils
  (:use clojure.contrib.swing-utils
        carneades.editor.utils.swing)
  (:import (carneades.editor.uicomponents.wizards.instantiatescheme 
             VariablePanel LiteralPanel)
           (carneades.editor.view.wizardsprotocol LiteralFormular)))

;; TODO delete suggestions arguments
(defn create-formular [formid literal-label literal suggestions variables suffix listeners args]
  (let [{:keys [form-listener
                previous-suggestion-listener
                next-suggestion-listener
                use-suggestion-listener]} listeners
                panel (LiteralPanel.)
                literalLabel (.literalLabel panel)
                literalText (.literalText panel)
                variablesPanel (.variablesPanel panel)
                suggestionText (.suggestionText panel)
                suggestionLabel (.suggestionLabel panel)
                previousSuggestionButton (.previousSuggestionButton panel)
                nextSuggestionButton (.nextSuggestionButton panel)
                useSuggestionButton (.useSuggestionButton panel)
                trigger (.dummyValidatorTrigger panel)
                textfields (mapcat (fn [var]
                                     (let [varpanel (VariablePanel.)
                                           label (.variableLabel varpanel)
                                           text (.variableText varpanel)]
                                       (.setText label (str var))
                                       (.setName text (str var "-" suffix))
                                       (.add variablesPanel varpanel)
                                       (when form-listener
                                         (add-key-released-listener
                                          text
                                          (fn [_] (apply form-listener formid var (.getText text) args)))) 
                                       [var {:text text :label label}]))
                                   variables)]
    (.setText literalLabel literal-label)
    (add-action-listener previousSuggestionButton
                         (fn [_] (apply previous-suggestion-listener formid args)))
    (add-action-listener nextSuggestionButton
                         (fn [_] (apply next-suggestion-listener formid args)))
    (add-action-listener useSuggestionButton
                         (fn [_] (apply use-suggestion-listener formid args)))
    (.setText literalText literal)
    (.setVisible trigger false)
    (LiteralFormular. formid panel (apply hash-map textfields))))

(defn set-literal-text [formular text]
  (let [panel (:panel formular)
        literalText (.literalText panel)]
    (.setText literalText text)))

(defn set-field-visible [formular var visible]
  (let [textfields (:textfields formular)
       {:keys [text label]} (get textfields var)]
    (.setVisible text visible)
    (.setVisible label visible)))

(defn fill-formular [formular var-values]
  (let [textfields (:textfields formular)
         panel (:panel formular)
         trigger (.dummyValidatorTrigger panel)]
     (doseq [[var value] var-values]
       (when-let [text (:text (get textfields var))]
         (.setText text value)))
     (.setText trigger "xyz")))