;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.editorapplication
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        [clojure.string :only (trim)]
        ;;
        ;; no imports of carneades.engine.* are allowed here
        ;;
        carneades.mapcomponent.map
        carneades.editor.view.viewprotocol
        carneades.editor.view.swinguiprotocol
        carneades.editor.view.menu.mainmenu
        carneades.editor.utils.swing
        carneades.editor.view.tabs
        carneades.editor.view.tree
        carneades.editor.view.search
        carneades.editor.view.properties.lkif
        carneades.editor.view.properties.statement
        carneades.editor.view.properties.argument
        carneades.editor.view.properties.premise
        carneades.editor.view.properties.graph
        carneades.editor.view.properties.properties
        carneades.editor.view.aboutbox
        carneades.editor.view.printpreview.preview)
  (:import (carneades.editor.uicomponents EditorApplicationView)
           (java.awt EventQueue)
           (javax.swing UIManager JFrame JFileChooser JOptionPane)
           (carneades.mapcomponent.map StatementCell ArgumentCell PremiseCell)))

(defvar- *frame* (EditorApplicationView/instance))
(defvar- *dialog-current-directory* (atom nil))
(defvar- *application-name* "Carneades Editor")

(defvar *openFileButton* (.openFileButton *frame*))
(defvar *openFileMenuItem* (.openFileMenuItem *frame*))
(defvar *closeFileMenuItem* (.closeFileMenuItem *frame*))

(defvar *exportFileMenuItem* (.exportFileMenuItem *frame*))
(defvar *printPreviewFileMenuItem* (.printPreviewFileMenuItem *frame*))
(defvar *printFileMenuItem* (.printFileMenuItem *frame*))
(defvar *aboutHelpMenuItem* (.aboutHelpMenuItem *frame*))

(defvar *closeLkifFileMenuItem* (.closeLkifFileMenuItem *frame*))
(defvar *exportLkifFileMenuItem* (.exportLkifFileMenuItem *frame*))

(defvar *openGraphMenuItem* (.openGraphMenuItem *frame*))
(defvar *closeGraphMenuItem* (.closeGraphMenuItem *frame*))
(defvar *exportGraphMenuItem* (.exportGraphMenuItem *frame*))

(defvar- *statement-selection-listeners* (atom ()))
(defvar- *argument-selection-listeners* (atom ()))
(defvar- *premise-selection-listeners* (atom ()))

(defn- node-selection-listener [path id obj]
  (cond (instance? StatementCell obj)
        (doseq [{:keys [listener args]} (deref *statement-selection-listeners*)]
          (apply listener path id (:stmt obj) args))

        (instance? ArgumentCell obj)
        (doseq [{:keys [listener args]} (deref *argument-selection-listeners*)]
          (apply listener path id (:arg obj) args))

        (instance? PremiseCell obj)
        (doseq [{:keys [listener args]} (deref *premise-selection-listeners*)]
          (apply listener path id (:pm obj) args))))

(deftype SwingView [] View SwingUI
  (init
   [this]
   (System/setProperty "apple.laf.useScreenMenuBar" "true")
   (System/setProperty "com.apple.mrj.application.apple.menu.about.name"
                       *application-name*)

   ;; make a new instance (without listeners!)
   (set-look-and-feel "Nimbus")
   (EditorApplicationView/reset)
   (lkif-properties-init)
   (graph-properties-init)
   (init-statement-properties)
   (init-premise-properties)
   
   (init-menu)
   (init-tree)
   (init-tabs)
   (init-search))

  (display-error
   [this title content]
   (JOptionPane/showMessageDialog *frame* content title
                                  JOptionPane/ERROR_MESSAGE))

  (ask-confirmation
   [this title content]
   (= (JOptionPane/showOptionDialog *frame* content title
                                  JOptionPane/OK_CANCEL_OPTION
                                  JOptionPane/QUESTION_MESSAGE
                                  nil nil nil)
      JOptionPane/OK_OPTION))
  
  (show
   [this]
   (disable-diagram-buttons-and-menus)
   (disable-file-items)
   (.setDefaultCloseOperation *frame* JFrame/DISPOSE_ON_CLOSE)
   (EventQueue/invokeLater
    (proxy [Runnable] []
      (run []
           (.setVisible *frame* true)))))

  (display-lkif-content
   [this file graphids]
   (add-lkif-content file graphids)
   (enable-file-items))

  (hide-lkif-content
   [this path]
   (remove-lkif-content path)
   (when-not (tree-has-content)
     (disable-file-items))
   (hide-properties))

  (open-graph
   [this path ag stmt-fmt]
   (let [title (get-tabtitle ag)]
     (let [component (get-component path (:id ag))]
       (if component
         (.setSelectedIndex *mapPanel*
                            (.indexOfComponent *mapPanel* component))
         (let [component (create-graph-component ag stmt-fmt)
               ;; ag (:arguments
               ;;   (first (solutions
               ;;           (engine
               ;;            '(hund-muss-neuangemeldet-werden ?h)))))
               component
               (create-graph-component ag
                 stmt-fmt)]
           ;; (printpreview component)
           (add-node-selection-listener component #(node-selection-listener
                                                    path (:id ag) %))
           (add-component component path (:id ag))
           (.add *mapPanel* title component)
           (.setTabComponentAt *mapPanel*
                               (.indexOfComponent *mapPanel* component)
                               (create-tabcomponent title))
           (.setSelectedComponent *mapPanel* component)
           (enable-diagram-buttons-and-menus))))))

  (close-graph
   [this path id]
   (let [component (get-component path id)]
     (.remove *mapPanel* component)
     (remove-component component))
   (when (tabs-empty?)
     (disable-diagram-buttons-and-menus)))

  (current-graph
   [this]
   (get-graphinfo (.getSelectedComponent *mapPanel*)))

  (display-lkif-property
   [this path]
   (show-properties (get-lkif-properties-panel path)))

  (display-graph-property
   [this id title mainissue]
   (show-properties (get-graph-properties-panel id title mainissue)))

  (display-statement-property
   [this stmt status proofstandard acceptable complement-acceptable]
   (show-properties (get-statement-properties-panel
                     stmt status proofstandard
                     acceptable complement-acceptable)))

  (display-premise-property
   [this polarity type]
   (show-properties (get-premise-properties-panel polarity type)))
    
  (display-argument-property
   [this id applicable weight direction scheme]
   (show-properties (get-argument-properties-panel id applicable
                                                   weight direction scheme)))

  (ask-file-to-save
   [this-view description extension suggested]
   (let [jc (proxy [JFileChooser] []
              (approveSelection
               []
               (when-let [selected (proxy-super getSelectedFile)]
                 (if (.exists selected)
                   (when (ask-confirmation this-view "Save"
                                           "Overwrite existing file?")
                     (proxy-super approveSelection))
                   (proxy-super approveSelection)))))]
     (.setFileFilter jc (create-file-filter description extension))
     (when-let [dir (deref *dialog-current-directory*)]
       (.setCurrentDirectory jc dir))
     (when suggested
       (.setSelectedFile jc suggested))
     (let [val (.showSaveDialog jc *frame*)]
       (when (= val JFileChooser/APPROVE_OPTION)
         (reset! *dialog-current-directory* (.getCurrentDirectory jc))
         (.getSelectedFile jc)))))

  (export-graph-to-svg
   [this ag stmt-fmt filename]
   (try
     (export-graph (create-graph-component ag stmt-fmt) filename)
     (catch java.io.IOException e
       (display-error this "Save error" (.getMessage e)))))
  
  (ask-lkif-file-to-open
   [this]
   (let [jc (JFileChooser.)]
     (.setFileFilter jc (create-file-filter "LKIF files" "xml"))
     (when-let [dir (deref *dialog-current-directory*)]
       (.setCurrentDirectory jc dir))
     (let [val (.showOpenDialog jc *frame*)]
       (when (= val JFileChooser/APPROVE_OPTION)
         (reset! *dialog-current-directory* (.getCurrentDirectory jc))
         (.getSelectedFile jc)))))
  
  (print-graph
   [this path ag stmt-fmt]
   (let [component (or (get-component path (:id ag))
                       (create-graph-component ag stmt-fmt))]
     (print-document component)))

  (print-preview
   [this path ag stmt-fmt]
   (let [component (or (get-component path (:id ag))
                       (create-graph-component ag stmt-fmt))]
     (printpreview *frame* component)))

  (display-about
   [this]
   (show-about-box *frame*))

  ;; here below, implementation of the SwingUI protocol:
  (add-close-button-listener
   [this f args]
   (register-close-button-listener f args))

  (add-open-file-button-listener
   [this f args]
   (apply add-action-listener *openFileButton* f args))

  (add-mousepressed-tree-listener
   [this f args]
   (apply add-mousepressed-listener *lkifsTree* f args))

  (add-open-file-menuitem-listener [this f args]
   (apply add-action-listener *openFileMenuItem* f args))

  (add-close-file-menuitem-listener
   [this f args]
   (apply add-action-listener *closeFileMenuItem* f args))

  (export-file-menuitem-listener
   [this f args]
   (apply add-action-listener *exportFileMenuItem* f args))

  (add-export-lkif-filemenuitem-listener
   [this f args]
   (apply add-action-listener *exportLkifFileMenuItem* f args))

  (add-export-graph-menuitem-listener
   [this f args]
   (apply add-action-listener *exportGraphMenuItem* f args))

  (add-about-helpmenuitem-listener
   [this f args]
   (apply add-action-listener *aboutHelpMenuItem* f args))

  (add-printpreview-filemenuitem-listener
   [this f args]
   (apply add-action-listener *printPreviewFileMenuItem* f args))

  (add-close-lkif-filemenuitem-listener
   [this f args]
   (apply add-action-listener *closeLkifFileMenuItem* f args))

  (add-export-filemenuitem-listener
   [this f args]
   (apply add-action-listener *exportFileMenuItem* f args))
  
  (add-open-graph-menuitem-listener
   [this f args]
   (apply add-action-listener *openGraphMenuItem* f args))

  (add-close-graph-menuitem-listener
   [this f args]
   (apply add-action-listener *closeGraphMenuItem* f args))

  (add-print-filemenuitem-listener
   [this f args]
   (apply add-action-listener *printFileMenuItem* f args))
  
  (add-search-button-listener
   [this f args]
   (apply add-action-listener *searchButton* f args))

  (get-selected-object-in-tree
   [this]
   (selected-object-in-tree))

  (get-graphinfo-being-closed
   [this event]
   (graphinfo-being-closed event))

  (get-searched-info
   [this]
   (let [text (.getSelectedItem *searchComboBox*)
            options {}]
     (if (nil? text)
       nil
       [(trim text) options])))

  (register-statement-selection-listener
   [this l args]
   (swap! *statement-selection-listeners* conj {:listener l :args args}))
  
  (register-argument-selection-listener
   [this l args]
   (swap! *argument-selection-listeners* conj {:listener l :args args}))
  
  (register-premise-selection-listener
   [this l args]
   (swap! *premise-selection-listeners* conj {:listener l :args args}))
  
  )
