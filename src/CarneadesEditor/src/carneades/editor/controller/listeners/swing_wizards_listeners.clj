;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.listeners.swing-wizards-listeners
  (:use carneades.editor.view.viewprotocol
        carneades.editor.view.wizardsprotocol
        (carneades.editor.utils state core)
        [carneades.engine.statement :only (statement? statement-formatted)]
        carneades.editor.view.swinguiprotocol
        carneades.editor.controller.handlers.messages
        (carneades.editor.controller.handlers goal-wizard
                                              findarguments-wizard
                                              instantiatescheme-wizard
                                              formalizestatement-wizard)))

;; if you plan to add a new wizard, take the formalize statement wizard as example
;; it is the one with the cleanest functional design

(defn- get-selected-statement [view path id]
  (when-let [node (get-selected-node view path id)]
    (when (statement? node)
      node)))

(defn findgoal-assistantmenuitem-listener [event view]
  (when-let [[path id] (current-graph view)]
    (when (on-pre-goalwizard view path id)
      (set-first-position-button-listener
       view (fn [event] (on-first-position view path id)) [])
      (set-previous-position-button-listener
       view (fn [event] (on-previous-position view path id)) [])
      (set-next-position-button-listener
       view (fn [event] (on-next-position view path id)) [])
      (set-last-position-button-listener
       view (fn [event] (on-last-position view path id)) [])
      (set-sort-by-listener view (fn [event]
                                   (on-sort-by view path id
                                               (get-sort-by-value view))) [])
      (set-minimize-button-listener view (fn [event]
                                           (on-minimize-positions
                                            view path id
                                            (get-minimize-value view))) [])
      (let [proponent-panel (get-proponent-panel view)
            abduction-panel (get-abduction-panel view)
            wizard (create-wizard view *goalwizard-title*
                                  [{:panel proponent-panel
                                    :desc *point-of-view*}

                                   {:panel abduction-panel
                                    :desc *statements*
                                    :validator on-abduction-panel-validation
                                    :listener on-abduction-panel
                                    :args [view path id]}]
                                  on-cancel-goal-wizard
                                  [view path id])
            settings (display-wizard view wizard)]
        (on-post-goalwizard view path id settings)))))

(defn findarguments-assistantmenuitem-listener [event view]
  (when-let [[path id] (current-graph view)]
    (when (on-pre-findarguments-wizard view path id
                                       (get-selected-statement view path id))
      (let [searchparameters-panel (get-searchparameters-panel view)
            searcharguments-panel (get-searcharguments-panel view)
            wizard (create-wizard view *findargumentswizard-title*
                                [{:panel searchparameters-panel
                                  :desc *search-parameters*}

                                 {:panel searcharguments-panel
                                  :desc *search-arguments*
                                  :validator on-searcharguments-panel-validation
                                  :listener on-searcharguments-panel
                                  :args [view path id]}
                                 ]
                                on-cancel-findarguments-wizard
                                [view path id])
            settings (display-wizard view wizard)]
        (on-post-findarguments-wizard view path id settings)))))

(defn state-wrapper [f state]
  (fn [settings]
    (swap! state assoc :settings settings)
    (state-call f state)))

(defn state-listener-wrapper [f state]
  (fn [event]
    (state-call f state)))

(defn instantiatescheme-assistantmenuitem-listener [event view]
  (let [[path id] (current-graph view)
        conclusion (get-selected-statement view path id)
        state (atom (create-instantiatescheme-state view path id conclusion))]
    (when-let [newstate (on-pre-instantiatescheme-wizard (deref state))]
      (reset! state newstate)
      (set-filter-text-listener
       view
       (fn [event]
         (swap! state assoc
                :filter-text (get-filter-text view)
                :conclusion-matches (and
                                     (conclusionmatches-button-enabled? view)
                                     (conclusionmatches-button-selected? view)))
         (state-call on-filter-schemes state))
       [])
      (set-conclusionmatches-button-listener
       view
       (fn [event]
         (swap! state assoc
                :conclusion-matches (conclusionmatches-button-selected? view))
         (state-call on-conclusionmatches state))
       [])
      (set-previous-clause-button-listener
       view (fn [event]
              (state-call on-previous-clause-button-listener state)) [])
      (set-next-clause-button-listener
       view (fn [event]
              (state-call on-next-clause-button-listener state)) [])
      (let [schemes-panel (get-schemes-panel view)
            clauses-panel (get-clauses-panel view)
            settings (display-branched-wizard
                      view [{:panel schemes-panel
                             :desc *schemes-panel*
                             :listener (state-wrapper on-schemes-panel state)
                             :validator schemes-panel-validator}
                            {:panel clauses-panel
                             :desc *clauses-panel-desc*
                             :id *clauses-id*
                             :listener (state-wrapper on-clauses-panel state)}]
                      (fn [settings stepid]
                        (swap! state assoc :settings settings)
                        (instantiatescheme-panel-selector state stepid))
                      [])]
        (when settings
          (swap! state assoc :settings settings)
          (state-call on-post-instantiatescheme-wizard state))))))

(defn formalizestatement-assistantmenuitem-listener [event view]
  (let [[path id] (current-graph view)
        statement (get-selected-statement view path id)
        state (atom {:view view
                     :path path
                     :id id
                     :statement statement})
        listeners {:form-listener nil
                   :previous-suggestion-listener
                   (state-listener-wrapper on-previous-suggestion-listener state)
                   :next-suggestion-listener
                   (state-listener-wrapper on-next-suggestion-listener state)
                   :use-suggestion-listener
                   (state-listener-wrapper on-use-suggestion-listener state)}
        [statement-panel formular]
        (get-statement-panel view (statement-formatted statement) listeners)]
    (swap! state assoc :formular formular)
    (when (state-call on-pre-formalizestatement-wizard state)
      (let [entitiespanel (get-entitiespanel view) 
            listener-wrapper (fn [event]
                               (swap! state assoc
                                      :classes-button-selected
                                      (classes-button-selected? view)
                                      :properties-button-selected
                                      (properties-button-selected? view)
                                      :filter-text
                                      (get-entities-filter-text view))
                               (state-call on-listener state))
            wizard (create-wizard view *formalizestatement-title*
                                  [{:panel entitiespanel
                                    :desc *entities-panel-desc*
                                    :listener (state-wrapper on-entities-panel state)
                                    :validator entities-panel-validator
                                    }
                                   {:panel statement-panel
                                    :desc *statement-panel-desc*
                                    :listener (state-wrapper on-statement-panel state)
                                    :validator statement-panel-validator
                                    }]
                                  (constantly true)
                                  [])]
        (do
          (set-classes-button-listener view listener-wrapper [])
          (set-properties-button-listener view listener-wrapper [])
          (set-filterentities-text-listener view listener-wrapper [])
          (when-let [settings (display-wizard view wizard 500 600)]
            (swap! state assoc :settings settings)
            (state-call on-post-formalizestatement-wizard state)))))))