;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.handlers.formalizestatement-wizard
  (:use clojure.pprint
        clojure.contrib.def
        carneades.editor.controller.documents
        carneades.editor.controller.handlers.messages
        carneades.editor.utils.core
        (carneades.editor.view wizardsprotocol viewprotocol)
        carneades.editor.controller.handlers.suggestions
        carneades.engine.statement
        [carneades.engine.argument-edit :only (update-statement-content)])
  (:require [carneades.engine.owl :as owl]
            [clojure.string :as str])
  (:import carneades.editor.view.wizardsprotocol.EntityItem))

(defn- filter-entities [entities keep-classes keep-properties filter-text]
  (let [filter-text (str/trim filter-text)
        filter-text (str/lower-case filter-text)
        entities (if (empty? filter-text)
                   entities
                   (filter #(.contains (str/lower-case %) filter-text) entities))
        entities (if keep-classes
                   entities
                   (filter #(not= (:type %) :class) entities))
        entities (if keep-properties
                   entities
                   (filter #(not= (:type %) :property) entities))]
    entities))

(defn on-pre-formalizestatement-wizard [state]
  (let [{:keys [view path id statement]} state
        reasoners (get-reasoners path)]
    (if (nil? statement)
      (do
        (display-error view *formalizestatementwizard-error*
                       *no-statementselected*)
        nil)
      (assoc state :reasoners reasoners))))

(defn on-entities-panel [state]
  (let [{:keys [view path id]} state
        reasoners (get-reasoners path)
        classes (mapcat
                 (fn [reasoner]
                   (map owl/to-symbol (owl/classes (owl/root-ontology reasoner))))
                 reasoners)
        properties (mapcat
                    (fn [reasoner]
                      (let [ontology (owl/root-ontology reasoner)]
                        (concat (map owl/to-symbol (owl/data-properties ontology))
                                (map owl/to-symbol (owl/object-properties ontology)))))
                    reasoners)
        entities (concat (map #(EntityItem. % (owl/shorten %) :class) classes)
                         (map #(EntityItem. % (owl/shorten %) :property) properties))]
    (display-entities view entities)
    (assoc state :entities entities)))

(defn on-listener [state]
  (let [{:keys [view entities classes-button-selected
                properties-button-selected filter-text]} state
                entities (filter-entities entities classes-button-selected
                                          properties-button-selected filter-text)]
    (display-entities view entities)
    state))

(defn entities-panel-validator [settings]
  (when (nil? (get settings "entity"))
    *select-an-entity*))

(defn on-statement-panel [state]
  (let [{:keys [view statement settings reasoners formular]} state
        entity (get settings "entity")
        suggestions (possible-individuals-statements
                     (if (= (:type entity) :class)
                       (list (:entity entity) '?x)
                       (list (:entity entity) '?x '?y))
                     reasoners)]
    (prepare-statement-formular view formular
                                (statement-formatted statement) entity)
    (if (empty? suggestions)
      (do
        (display-no-suggestion view formular)
        (assoc state :entity entity :suggestions nil))
      (do
        (display-suggestion view formular (statement-formatted (first suggestions)) 1 (count suggestions))
        (assoc state
          :entity
          entity
          :suggestions {:current-idx 0 :values suggestions :size (count suggestions)})))))

(defn statement-panel-validator [settings]
  (let [x (get settings "?x-")
        y (get settings "?y-")
        xval (str-term x)
        yval (str-term y)
        entity (get settings "entity")]
    (condp = (:type entity)
        :class (cond (empty? x)
                     *fillin-form*

                     (nil? xval)
                     *invalid-content*)
        :property (cond (or (empty? x) (empty? y))
                        *fillin-form*
                        
                        (or (nil? xval) (nil? yval))
                        *invalid-content*))))

(defn on-previous-suggestion-listener [state]
  (m-let [{:keys [view formular suggestions]} state
          {:keys [current-idx values size]} suggestions]
    (when (pos? current-idx)
      (let [idx (dec current-idx)
            current (nth values idx)
            suggestions (assoc suggestions :current-idx idx)]
        (display-suggestion view formular (statement-formatted current) (inc idx) size)
        (assoc state :suggestions suggestions)))))

(defn on-next-suggestion-listener [state]
  (m-let [{:keys [view formular suggestions]} state
          {:keys [current-idx values size]} suggestions]
    (when (not= current-idx (dec size))
      (let [idx (inc current-idx)
            current (nth values idx)
            suggestions (assoc suggestions :current-idx idx)]
        (display-suggestion view formular (statement-formatted current) (inc idx) size)
        (assoc state :suggestions suggestions)))))

(defn on-use-suggestion-listener [state]
  (m-let [{:keys [view formular suggestions]} state
          {:keys [current-idx values]} suggestions
          suggestion (nth values current-idx)
          values (filter (complement variable?) (term-args suggestion))
          formatted-values (map term-str values)
          var-values (partition 2 (interleave (if (= (count values) 1)
                                                '[?x]
                                                '[?x ?y]) formatted-values))]
    (fillin-formular view formular var-values)
    state))

(defn on-post-formalizestatement-wizard [state]
  (let [{:keys [view path id statement settings entity]} state
        x (get settings "?x-")
        y (get settings "?y-")
        xval (str-term x)
        yval (str-term y)
        stmt (condp = (:type entity)
                 :class (list (:entity entity) xval)
                 :property (list (:entity entity) xval yval))]
    (m-let [ag (get-ag path id)
            ag (update-statement-content ag statement stmt)]
      (do-ag-update view [path :ags (:id ag)] ag)
      (graph-changed view path ag statement-formatted)
      (display-statement view path ag stmt statement-formatted))))

