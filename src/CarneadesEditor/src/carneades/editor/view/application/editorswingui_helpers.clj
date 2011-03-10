;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Helper functions for the implementation of the SwingUI protocol"}
  carneades.editor.view.application.editorswingui-helpers
  (:use clojure.contrib.def)
  (:import java.awt.BorderLayout
           (org.netbeans.spi.wizard WizardPage WizardPage$WizardResultProducer
                                    WizardObserver WizardBranchController)
           org.netbeans.api.wizard.WizardDisplayer))

(defn create-wizardpages [panels]
  (map (fn [panel]
         (let [{:keys [panel desc validator listener args id]} panel
               id (if (nil? id) (str (gensym desc)) id)
               wizardpage
               (proxy [WizardPage] [id desc]
                 (validateContents
                  [comp event]
                  (if validator
                    (apply validator (proxy-super getWizardDataMap) args)
                    nil)))]
           (doto wizardpage
             (.setLayout (BorderLayout.))
             (.add panel))
           {:desc desc :id id :page wizardpage :listener listener :args args}))
       panels))

(defn create-wizardobserver [wizardpages]
  (reify WizardObserver
    (navigabilityChanged [this wizard])
    (selectionChanged
     [this wizard]
     (let [stepid (.getCurrentStep wizard)]
       (when-let [pagedata (first (filter #(= (:id %) stepid) wizardpages))]
         (when-let [listener (:listener pagedata)]
           (let [args (:args pagedata)
                 wizardpage (:page pagedata)
                 datamap (.getWizardDataMap wizardpage)]
             (apply listener datamap args))))))
    (stepsChanged [this wizard])))