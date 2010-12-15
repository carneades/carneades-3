;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.handlers.instantiatescheme-wizard
  (:use clojure.contrib.def
        (carneades.engine statement argument)
        carneades.editor.utils.core
        [carneades.engine.unify :only (unify apply-substitution)]
        carneades.editor.controller.handlers.messages
        clojure.contrib.pprint
        (carneades.editor.view wizardsprotocol viewprotocol swinguiprotocol)
        carneades.editor.controller.documents)
  (:require [clojure.string :as str]))

(defvar *clauses-id* (str (gensym "clauses-panel-id")))

(defvar- *conclusion* (atom nil))
(defvar- *conclusion-matches* (atom false))
(defvar- *clauses* (atom nil))

(defvar- *literal-wizards* (atom {}))
(defvar- *formulars* (atom {}))

(defvar- *current-substitution* (atom {}))

(defn- rules-name [rules]
  (map #(str (:id %)) rules))

(defn on-schemes-panel [settings view path id]
  (let [rules (get-rules path)]
    (display-schemes view (rules-name rules))))

(defn on-pre-instantiatescheme-wizard [view path id conclusion]
  (on-schemes-panel nil view path id)
  (if (nil? conclusion)
    (do
      (reset! *conclusion* nil)
      (set-conclusion-statement view (statement-formatted ""))
      (set-conclusionmatches-button-enabled view false))
    (do
      (reset! *conclusion* conclusion)
      (set-conclusion-statement view (statement-formatted conclusion))
      (set-conclusionmatches-button-enabled view true))))

(defn- get-unifiable-rules [rules conclusion]
  (if conclusion
    (filter #(do
               ;; TODO: multiple conclusion?
               ;; (printf "unify %s and %s\n" (first (:head %)) conclusion)
               (unify (first (:head %)) conclusion)) rules)
    rules))

(defn- filter-schemes [rules filter-text conclusionmatches]
  (let [rules (if conclusionmatches
                (get-unifiable-rules rules (deref *conclusion*))
                rules)
        names (rules-name rules)]
    (if (empty? filter-text)
      names
      (filter #(.contains (str/lower-case %) filter-text) names))))

(defn on-filter-schemes [view path id text conclusionmatches]
  (prn "on-filter-schemes")
  (prn "text =")
  (prn text)
  (let [text (str/lower-case (str/trim text))
        names (filter-schemes (get-rules path) text conclusionmatches)]
    (display-schemes view names)))

(defn schemes-panel-validator [settings view path id]
  (let [scheme (get settings "scheme")]
    (when (nil? scheme)
      *select-a-scheme*)))

(defn on-conclusionmatches [view path id filter-text conclusionmatches]
  (reset! *conclusion-matches* conclusionmatches)
  (let [names (filter-schemes (get-rules path) filter-text conclusionmatches)]
   (display-schemes view names)))

(defn get-rule [path scheme]
  (first (filter #(= (str (:id %)) scheme) (get-rules path))))

(defn on-clauses-panel [settings view path id]
  (prn "on-clauses-panel")
  (reset! *literal-wizards* {})
  (reset! *formulars* {})
  (let [rule (get-rule path (get settings "scheme")) 
        clauses (:body rule)
        clause (first clauses)
        nb-clauses (count clauses)
        conclusion-matches (deref *conclusion-matches*)]
    (prn "rule =")
    (prn rule)
    (if conclusion-matches
      (reset! *current-substitution* (unify (first (:head rule)) (deref *conclusion*)))
      (reset! *current-substitution* nil))
    (reset! *clauses* {:clauses (apply vector clauses) :index 0 :nclauses nb-clauses})
    (display-clause view clause 0 nb-clauses statement-formatted))
  )

(defn on-previous-clause-button-listener [view path id]
  (when-let* [clauses (deref *clauses*)
              {:keys [clauses index nclauses]} clauses]
    (when (pos? index)
      (let [index (dec index)
            clause (get clauses index)]
        (swap! *clauses* assoc :index index)
        (display-clause view clause index nclauses statement-formatted)))))

(defn on-next-clause-button-listener [view path id]
  (when-let* [clauses (deref *clauses*)
              {:keys [clauses index nclauses]} clauses]
    (when (< index (dec nclauses))
      (let [index (inc index)
            clause (get clauses index)]
        (swap! *clauses* assoc :index index)
        (display-clause view clause index nclauses statement-formatted)))))

(defn gen-form-id [clause-number literal-nb]
  (str clause-number "-" (gensym literal-nb)))

(defn form-listener [formid val value view]
  (prn "form-listener")
  (printf "%s -> %s\n" val value)
  (prn "value =")
  (prn value)
  (prn "")
  (let [form (get (deref *formulars*) formid)]
    (if (empty? value)
      (swap! *current-substitution* dissoc val)
      (swap! *current-substitution* assoc val value))
    (doseq [form (vals (deref *formulars*))]
      (fillin-formular view form [[val (statement-formatted value)]])))
  )

(defvar- *suggestions* (atom nil))

(defn previous-suggestion [formid view]
  (when-let* [form (get (deref *formulars*) formid)
              suggestions (deref *suggestions*)
              {:keys [current-idx suggestions size]} suggestions]
    (when (pos? current-idx)
      (let [idx (dec current-idx)
            current (nth suggestions idx)]
        (swap! *suggestions* assoc :current-idx idx)
        (display-suggestion view form (statement-formatted current) (inc idx) size)))))

(defn next-suggestion [formid view]
  (when-let* [form (get (deref *formulars*) formid)
              suggestions (deref *suggestions*)
              {:keys [current-idx suggestions size]} suggestions]
    (when (not= current-idx (dec size))
      (let [idx (inc current-idx)
            current (nth suggestions idx)]
        (swap! *suggestions* assoc :current-idx idx)
        (display-suggestion view form (statement-formatted current) (inc idx) size)))))

(defn use-suggestion [view formid vars]
  (when-let* [form (get (deref *formulars*) formid)
              suggestions (deref *suggestions*)
              {:keys [current-idx suggestions]} suggestions
              current (nth suggestions current-idx)
              values (map str (filter (complement variable?) (term-args current)))
              var-values (partition 2 (interleave vars values))]
    (swap! *current-substitution* merge (apply hash-map (apply concat var-values)))
    (fillin-formular view form var-values)))

(defn get-literal-formular [view clause-number literal literal-nb]
  (let [formid (gen-form-id clause-number literal-nb)]
    (if-let [panel (get (deref *formulars*) formid)]
      panel
      (let [vars (variables literal)
            form (create-literal-formular view
                                          formid (statement-formatted literal) []
                                          vars
                                          (str literal-nb)
                                          {:form-listener form-listener
                                           :previous-suggestion-listener previous-suggestion
                                           :next-suggestion-listener next-suggestion
                                           :use-suggestion-listener
                                           (fn [formid view]
                                             (use-suggestion view formid vars))}
                                          [view])]
        (swap! *formulars* assoc formid form)
        form))))

(defn get-desc [form idx]
  (format *complete-form-n* idx))

(defn create-literal-validator [literal idx]
  (fn [settings]
    (prn "validator")
    (prn "settings =")
    (prn settings)
    (let [variables (filter variable? literal)
          formvariables (keep (fn [var]
                                (let [name (str var "-" "";; idx
                                                )
                                      val (get settings name)]
                                  (when (not (empty? val))
                                    [var val])))
                               variables)
          current-substitution (deref *current-substitution*)
          ;; validator is called before our listener so we need to merge the values
          sub (merge current-substitution (apply hash-map (apply concat formvariables)))]
      (printf "current-sub = %s\n" sub)
      (if (ground? (apply-substitution sub literal))
        nil
        *fillin-form*))))

(defn unifiable-statements [ag literal current-subs]
  (prn "unifiable-statements")
  (prn "literal =")
  (prn literal)
  (let [stmts (map node-statement (get-nodes ag))
        suggestions (filter #(unify literal %) stmts)]
    (prn "suggestions =")
    (pprint suggestions)
    
    suggestions
    ))

(defn on-literal-panel [view path id form literal settings]
  (prn "on-literal-panel")
  (let [ag (get-ag path id)
        current-subs (deref *current-substitution*)
        var-values (map (fn [[var values]] [var (statement-formatted values)]) current-subs)]
    (fillin-formular view form var-values)
    (let [suggestions (unifiable-statements ag literal current-subs)
          size (count suggestions)]
      (if (zero? size)
        (reset! *suggestions* nil)
        (reset! *suggestions* {:current-idx 0 :suggestions suggestions :size size}))
      (when-not (empty? suggestions)
        (display-suggestion view form (statement-formatted (first suggestions)) 1 size)))
    )
  )

(defn get-literals-wizard [view path id clause-number]
  (if-let [wizard (get (deref *literal-wizards*) clause-number)]
    wizard
    (when-let* [clauses (deref *clauses*)
                {:keys [clauses index nclauses]} clauses
                literals (get clauses clause-number)
                forms (map-indexed (fn [idx literal]
                                     (get-literal-formular view clause-number literal "";; idx
                                                           ))
                                   literals) 
                wizard (create-wizard view ""
                                      (map-indexed (fn [idx form]
                                                     (let [literal (nth literals idx)]
                                                       {:panel (:panel form)
                                                        :desc (get-desc form (inc idx))
                                                        :validator
                                                        (create-literal-validator
                                                         literal idx)
                                                        :listener #(on-literal-panel view path id form
                                                                                     literal %)
                                                       }))
                                                   forms)
                                      (constantly true)
                                      [])]
      (swap! *literal-wizards* assoc clause-number wizard)
      wizard
      ))
  )

(defn instantiatescheme-panel-selector [settings stepid view path id]
  (condp = stepid
      *clauses-id* (let [clause-number (get settings "clause-number")]
                     (if (empty? clause-number)
                       nil
                       (let [wizard
                             (get-literals-wizard
                              view path id
                              (Integer/parseInt clause-number))]
                         wizard)))
      
      nil))

(defn instantiate-scheme [view path id scheme conclusion premises]
  (when-let* [ag (get-ag path id)
              ag (update-statement ag conclusion)
              ag (reduce (fn [ag premise]
                           (update-statement ag premise)) ag premises)
              arg (argument (gen-argument-id ag) :pro conclusion (map #(pm %) premises) scheme)
              ag (assert-argument ag arg)]
    (do-update-section view [path :ags (:id ag)] ag)
    (graph-changed view path ag statement-formatted)
    (display-statement view path ag conclusion statement-formatted)))

(defn on-post-instantiatescheme-wizard [view path id settings]
  (let [rule (get-rule path (get settings "scheme"))
        clause-number (Integer/parseInt (get settings "clause-number"))
        current-subs (deref *current-substitution*)
        clause (nth (:body rule) clause-number)
        clause (map #(apply-substitution current-subs %) clause)
        head (:head rule)
        head (map #(apply-substitution current-subs %) head)]
    (instantiate-scheme view path id (str (:id rule)) (first head) clause)))
