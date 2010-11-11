;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.application.editorapplication
  (:use carneades.editor.utils.type
        (carneades.editor.view viewprotocol swinguiprotocol wizardsprotocol)
        (carneades.editor.view.application editorview editorswingui)
        (carneades.editor.view.application.wizards goal findarguments))
  (:import carneades.editor.view.application.editorview.EditorView
           carneades.editor.view.application.editorswingui.EditorSwingUI
           carneades.editor.view.application.wizards.goal.EditorSwingGoalWizard
           carneades.editor.view.application.wizards.findarguments.EditorSwingFindArgumentsWizard))

(defrecord SwingView [view swingui swinggoalwizard swingfindargumentswizard])

(auto-extend SwingView carneades.editor.view.viewprotocol/View
             (:view this))
(auto-extend SwingView carneades.editor.view.swinguiprotocol/SwingUI
             (:swingui this))
(auto-extend SwingView carneades.editor.view.wizardsprotocol/SwingGoalWizard
             (:swinggoalwizard this))
(auto-extend SwingView carneades.editor.view.wizardsprotocol/SwingFindArgumentsWizard
             (:swingfindargumentswizard this))

(defn create-swingview []
  (let [view (EditorView.)
        swingui (EditorSwingUI.)
        swinggoalwizard (EditorSwingGoalWizard.)
        swingfindargumentswizard (EditorSwingFindArgumentsWizard.)]
    (SwingView. view swingui swinggoalwizard swingfindargumentswizard)))
