;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.application.editorswingui-helpers
  (:use clojure.contrib.def)
  (:import java.awt.BorderLayout
           (org.netbeans.spi.wizard WizardPage WizardPage$WizardResultProducer
                                    WizardObserver WizardBranchController)
           org.netbeans.api.wizard.WizardDisplayer))

(defn create-wizardpages [panels]
  (map (fn [panel]
         (let [{:keys [panel desc validator listener args]} panel
               wizardpage
               (proxy [WizardPage] [desc]
                 (validateContents
                  [comp event]
                  (if validator
                    (apply validator (proxy-super getWizardDataMap) args)
                    nil))
                 (getWizardDataMap
                  []
                  (proxy-super getWizardDataMap)))]
           (doto wizardpage
             (.setLayout (BorderLayout.))
             (.add panel))
           {:desc desc :page wizardpage :listener listener :args args}))
       panels))
