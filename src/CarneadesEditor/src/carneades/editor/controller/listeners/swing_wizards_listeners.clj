;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.listeners.swing-wizards-listeners
  (:use carneades.editor.view.viewprotocol
        carneades.editor.view.wizardsprotocol
        carneades.editor.view.swinguiprotocol
        carneades.editor.controller.handlers.messages
        (carneades.editor.controller.handlers goal-wizard
                                              findarguments-wizard)))

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
  (prn "findarguments-assistantmenuitem-listener")
  (when-let [[path id] (current-graph view)]
    (when (on-pre-findarguments-wizard view path id)
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
