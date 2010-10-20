;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.listeners.swing-wizards-listeners
  (:use carneades.editor.view.viewprotocol
        carneades.editor.view.wizardsprotocol
        carneades.editor.view.swinguiprotocol
        carneades.editor.controller.handlers.messages
        carneades.editor.controller.handlers.wizards))

(defn findgoal-assistantmenuitem-listener [event view]
  (prn "findgoal-assistantmenuitem-listener")
  (when-let [[path id] (current-graph view)]
    (when (on-pre-goalwizard view path id)
      (let [proponent-panel (get-proponent-panel view)
            abduction-panel (get-abduction-panel view)
            statements-panel (get-statements-panel view)
            res (display-wizard view *goalwizard-title*
                        [{:panel proponent-panel
                          :desc *point-of-view*
                          :validator (fn [settings]
                                       (on-proponent-panel-validation view path id settings))}

                         {:panel abduction-panel
                          :desc *positions*
                          :validator (fn [settings]
                                       (on-abduction-panel-validation view path id settings))
                          :listener (fn [settings]
                                      (on-abduction-panel view path id settings))}

                         {:panel statements-panel
                          :desc *statements*
                          :validator (fn [settings]
                                       (on-statements-panel-validation view path id settings))
                          :listener (fn [settings]
                                      (on-statements-panel view path id settings))}])
            statement (get res "statements")]
        (prn "selected = ")
        (prn statement)
        (on-post-goalwizard view path id statement)))))
