;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.application.editorview
  (:use (clojure.contrib def swing-utils)
        carneades.ui.diagram.graphvizviewer
        (carneades.editor.view.dialogs statement-editor location)
        carneades.editor.view.application.context
        carneades.editor.utils.swing
        carneades.editor.view.application.editor-helpers
        carneades.editor.view.viewprotocol
        carneades.editor.view.printing.preview
        carneades.editor.view.dialogs.aboutbox
        carneades.editor.utils.core
        (carneades.editor.view.components uicomponents search tabs)
        (carneades.editor.view.properties lkif statement argument premise graph
                                          properties)
        (carneades.mapcomponent map map-edit))
  (:require [carneades.editor.view.components.tree :as tree])
  (:import java.io.File
           (carneades.editor.uicomponents EditorApplicationView ReadSentenceDialog)
           (java.awt EventQueue Cursor)
           (javax.swing JFileChooser JOptionPane)))

(defvar- *application-name* "Carneades Editor")
(defvar- *dialog-current-directory* (atom nil))

(deftype EditorView []
  View
  (init
   [this]
   (System/setProperty "apple.laf.useScreenMenuBar" "true")
   (System/setProperty "com.apple.mrj.application.apple.menu.about.name"
                       *application-name*)
   (set-look-and-feel "Nimbus")
   
   ;; make a new instance (without listeners!)
   (EditorApplicationView/reset)
   (add-action-listener *zoomInButton* on-zoom-in)
   (add-action-listener *zoomOutButton* on-zoom-out)
   (add-action-listener *zoomResetButton* on-zoom-reset)
   (add-action-listener *selectAllEditMenuItem* select-all-listener this)
   (add-action-listener *addExistingPremiseMenuItem*
                        add-existing-premise-menuitem-listener this)
   (add-action-listener *editStatementMenuItem* statement-edit-menuitem-listener this)
   
   (init-lkif-properties)
   (init-graph-properties)
   (init-statement-properties)
   (init-premise-properties)
   (tree/init-tree)
   (init-tabs)
   (init-search)
   (init-context))

  (show
   [this]
   (do-swing-and-wait
    (.setVisible *frame* true)))

  (open-graph
   [this path ag stmt-fmt]
   (swap! *main-issues* assoc [path (:id ag)] (:main-issue ag))
   (let [component (get-component path (:id ag))]
     (if component
       (do
         (select-component component)
         (set-current-ag-context path (:id ag)))
       (create-tabgraph-component this path ag stmt-fmt))
     (tree/select-ag path (:id ag))))

  (redisplay-graph
   [this path ag stmt-fmt]
   (when-let [component (get-component path (:id ag))]
     (layout-map component)))

  (graph-changed
   [this path ag stmt-fmt]
   (when-let [component (get-component path (:id ag))]
     (replace-graph component ag stmt-fmt)))
  
  (close-graph
   [this path id isfresh]
   (let [component (get-component path id)]
     (remove-component component)
     (remove-ag-context path id)
     (when isfresh
       (tree/remove-ag path id)))
   (if (tabs-empty?)
     (set-current-ag-context-empty)
     (let [[path id] (current-graph this)]
       (set-current-ag-context path id))))

  (current-graph
   [this]
   (get-graphinfo (.getSelectedComponent *mapPanel*)))

  (opened-graphs
   [this]
   (let [ntab (.getTabCount *mapPanel*)]
     (map #(get-graphinfo (.getComponentAt *mapPanel* %)) (range ntab))))

  (ask-file-to-open
   [this desc exts]
   (let [jc (JFileChooser.)]
     (.setFileFilter jc (create-file-filter desc exts))
     (when-let [dir (deref *dialog-current-directory*)]
       (.setCurrentDirectory jc dir))
     (let [val (.showOpenDialog jc *frame*)]
       (when (= val JFileChooser/APPROVE_OPTION)
         (reset! *dialog-current-directory* (.getCurrentDirectory jc))
         (.getSelectedFile jc)))))

  (ask-location-to-open
   [this]
   (show-location-dialog this))

  (ask-file-to-save
   [this-view descriptions suggested]
   (letfn [(changeextension
            [file ext]
            (let [filename (.getName file)]
             (let [idxlastdot (.lastIndexOf filename ".")]
               (if (not= idxlastdot -1)
                 (File. (str (subs filename 0 idxlastdot) "." ext))
                 (File. (str filename "." ext))))))]
     (let [selectedfile (atom suggested)
           jc (proxy [JFileChooser] []
                (approveSelection
                 []
                 (when-let [selected (proxy-super getSelectedFile)]
                   (if (.exists selected)
                     (when (ask-confirmation this-view "Save"
                                             "Overwrite existing file?")
                       (proxy-super approveSelection))
                     (proxy-super approveSelection)))))]
       (doseq [[description extension] descriptions]
         (.addChoosableFileFilter jc (create-file-filter description (set [extension]))))
       (when-let [dir (deref *dialog-current-directory*)]
         (.setCurrentDirectory jc dir))
       (when suggested
         (.setSelectedFile jc suggested))
       (add-propertychange-listener
        jc
        (fn [event]
          (condp = (.getPropertyName event)
              
              JFileChooser/FILE_FILTER_CHANGED_PROPERTY
            (when-let [file (deref selectedfile)]
              (let [desc (.. jc getFileFilter getDescription)]
                (.setSelectedFile jc (changeextension file
                                                      (descriptions desc)))))
            
            JFileChooser/SELECTED_FILE_CHANGED_PROPERTY
            (do
              (when-let [file (.. event getNewValue)]
                (reset! selectedfile file)))

            nil)))
       (let [val (.showSaveDialog jc *frame*)]
         (when (= val JFileChooser/APPROVE_OPTION)
           (reset! *dialog-current-directory* (.getCurrentDirectory jc))
           [(.getSelectedFile jc) (.. jc getFileFilter getDescription)])))))

  (export-graph-to-svg
   [this ag stmt-fmt filename]
   (try
     (export-graph (create-graph-component ag stmt-fmt) filename)
     (catch java.io.IOException e
       (display-error this "Save error" (.getMessage e)))))

  (export-graph-to-dot
   [this ag statement-formatted filename]
   (try
     (spit filename (gen-graphvizcontent ag statement-formatted))
     (catch java.io.IOException e
       (display-error this "Save error" (.getMessage e)))))

  (export-graph-to-graphviz-svg
   [this ag statement-formatted filename]
   (prn "export to graphviz svg")
   (try
     (gen-image ag statement-formatted filename)
     (catch java.io.IOException e
       (display-error this "Save error" (.getMessage e)))))

  (display-lkif-content
   [this path filename graphinfos]
   (tree/add-lkif-content path filename graphinfos)
   (set-current-lkif-context path))

  (hide-lkif-content
   [this path]
   (tree/remove-lkif-content path)
   (when-not (tree/has-content)
     (set-current-lkif-context-empty))
   (hide-properties))

  (print-preview
   [this path ag stmt-fmt]
   (let [component (or (get-component path (:id ag))
                       (create-graph-component ag stmt-fmt))]
     (printpreview *frame* (:component component))))

  (print-graph
   [this path ag stmt-fmt]
   (let [component (or (get-component path (:id ag))
                       (create-graph-component ag stmt-fmt))]
     (print-document (:component component))))

  (display-lkif-property
   [this path importurls]
   (show-properties (get-lkif-properties-panel path importurls)))

  (display-graph-property
   [this path id title mainissue]
   (show-properties (get-graph-properties-panel path id title mainissue)))

  (display-about
   [this]
   (show-about-box *frame*))

  (ask-confirmation
   [this title content]
   (= (JOptionPane/showOptionDialog *frame* content title
                                  JOptionPane/OK_CANCEL_OPTION
                                  JOptionPane/QUESTION_MESSAGE
                                  nil nil nil)
      JOptionPane/OK_OPTION))

  (ask-yesnocancel-question
   [this title content]
   (condp = (JOptionPane/showOptionDialog *frame* content title
                                          JOptionPane/YES_NO_CANCEL_OPTION
                                          JOptionPane/QUESTION_MESSAGE
                                          nil nil nil)
       JOptionPane/YES_OPTION :yes
       JOptionPane/NO_OPTION :no
       JOptionPane/CANCEL_OPTION :cancel
       JOptionPane/CLOSED_OPTION :cancel))

  (read-sentence
   [this title content]
   (let [dialog (ReadSentenceDialog. *frame* true)
         okbutton (.okButton dialog)
         textField (.textField dialog)
         textLabel (.textLabel dialog)
         textcontent (atom nil)]
     (.setTitle dialog title)
     (.setText textLabel content)
     (add-action-listener okbutton (fn [event]
                              (reset! textcontent (.getText textField))))
     (.setLocationRelativeTo dialog *frame*)
     (.setVisible dialog true)
     (deref textcontent)))

  (read-statement
   [this content]
   (show-statement-editor content false))

  (display-error
   [this title content]
   (JOptionPane/showMessageDialog *frame* content title
                                  JOptionPane/ERROR_MESSAGE))
  (display-statement-property
   [this path id maptitle stmt stmt-fmt status proofstandard acceptable complement-acceptable]
   (show-properties (get-statement-properties-panel
                     path id maptitle
                     stmt stmt-fmt status proofstandard
                     acceptable complement-acceptable)))
  
  (set-current-statement-property
   [this path id maptitle stmt stmt-fmt status proofstandard acceptable complement-acceptable]
   (swap! *statement-being-edited-menu-info* assoc
          :content stmt
          :previous-content stmt
          :previous-status status)
   (.setSelected *statedMenuItem* (= status :stated))
   (.setSelected *questionedMenuItem* (= status :questioned))
   (.setSelected *acceptedMenuItem* (= status :accepted))
   (.setSelected *rejectedMenuItem* (= status :rejected)))

  (display-premise-property
   [this path id maptitle arg polarity type role atom]
   (show-properties
    (get-premise-properties-panel path id maptitle arg polarity type role atom)))

  (display-argument-property
   [this path id maptitle argid title applicable weight direction scheme]
   (show-properties (get-argument-properties-panel path id maptitle argid title applicable
                                                   weight direction scheme)))

  (display-search-state
   [this inprogress]
   (set-search-state inprogress))

  (display-statement-search-result
   [this path id stmt stmt-fmt]
   (add-stmt-search-result path id stmt stmt-fmt))

  (display-statement
   [this path ag stmt stmt-fmt]
   (prn "display statement")
   (open-graph this path ag stmt-fmt)
   (let [component (get-component path (:id ag))]
     (select-statement component stmt stmt-fmt)))

  (display-argument
   [this path ag arg stmt-fmt]
   (open-graph this path ag stmt-fmt)
   (let [component (get-component path (:id ag))]
     (select-argument component arg)))
  
  (display-argument-search-result
   [this path id arg title]
   (add-arg-search-result path id arg title))
  
  (set-busy
   [this isbusy]
   (if isbusy
     (.setCursor *frame* Cursor/WAIT_CURSOR)
     (.setCursor *frame* (Cursor/getDefaultCursor))))

  (edit-undone
   [this path id]
   (when-let [component (get-component path id)]
     (undo component)))

  (edit-redone
   [this path id]
   (when-let [component (get-component path id)]
     (redo component)))

  (set-can-undo
   [this path id state]
   (set-ag-canundo path id state))

  (set-can-redo
   [this path id state]
   (set-ag-canredo path id state))

  (set-dirty
   [this path ag state]
   (tree/set-lkif-dirty path state)
   (set-ag-dirty path (:id ag) state))

  (set-lkif-dirty
   [this path state]
   (tree/set-lkif-dirty path state))

  (copyselection-clipboard
   [this path id]
   (when-let [component (get-component path id)]
     (copyselection-toclipboard component)))
  
  (statement-content-changed
   [this path ag oldstmt newstmt]
   (when-let [component (get-component path (:id ag))]
     (change-statement-content component ag oldstmt newstmt)))

  (statement-status-changed
   [this path ag stmt]
   (prn "statement-statuts-changed: stmt =")
   (prn stmt)
   (when-let [component (get-component path (:id ag))]
     (change-statement-status component ag stmt)))

  (statement-proofstandard-changed
   [this path ag stmt]
   (when-let [component (get-component path (:id ag))]
    (change-statement-proofstandard component ag stmt)))

  (title-changed
   [this path ag title]
   (when-let [component (get-component path (:id ag))]
     (change-title component ag title)
     (change-tab-title component title))
   (tree/change-ag-title path (:id ag) title))

  (premise-polarity-changed
   [this path ag oldarg arg pm]
   (when-let [component (get-component path (:id ag))]
    (change-premise-polarity component ag oldarg arg pm)))

  (premise-type-changed
   [this path ag oldarg arg pm]
   (when-let [component (get-component path (:id ag))]
     (change-premise-type component ag oldarg arg pm)))

  (premise-role-changed
   [this path ag oldarg arg pm]
   (when-let [component (get-component path (:id ag))]
     (change-premise-role component ag oldarg arg pm)))

  (argument-title-changed
   [this path ag arg title]
   (when-let [component (get-component path (:id ag))]
     (change-argument-title component ag arg title)))

  (argument-weight-changed
   [this path ag arg weight]
   (when-let [component (get-component path (:id ag))]
     (change-argument-weight component ag arg weight)))

  (argument-direction-changed
   [this path ag arg direction]
   (when-let [component (get-component path (:id ag))]
     (change-argument-direction component ag arg direction)))

  (premise-added
   [this path ag arg stmt]
   (when-let [component (get-component path (:id ag))]
     (add-premise component ag arg stmt)))

  (premise-deleted
   [this path ag arg pm]
   (when-let [component (get-component path (:id ag))]
     (delete-premise component ag arg pm)))

  (statement-deleted
   [this path ag stmt]
   (when-let [component (get-component path (:id ag))]
     (delete-statement component ag stmt)))

  (argument-deleted
   [this path ag arg]
   (when-let [component (get-component path (:id ag))]
     (delete-argument component ag arg)))

  (new-premise-added
   [this path ag arg stmt stmt-str]
   (when-let [component (get-component path (:id ag))]
     (add-new-premise component ag arg stmt stmt-str)))

  (mainissue-changed
   [this path ag stmt]
   (swap! *main-issues* assoc [path (:id ag)] (:main-issue ag))
   (when-let [component (get-component path (:id ag))]
     (change-mainissue component ag stmt)))

  (new-statement-added
   [this path ag stmt stmt-formatted]
   (when-let [component (get-component path (:id ag))]
     (add-new-statement component ag stmt stmt-formatted)))

  (new-argument-added
   [this path ag arg]
   (when-let [component (get-component path (:id ag))]
     (add-new-argument component ag arg)))

  (new-graph-added
   [this path ag stmt-fmt]
   (tree/add-ag path (:id ag) (:title ag)))

  (graph-deleted
   [this path id]
   (tree/remove-ag path id))

  (register-statement-selection-listener
   [this l args]
   (swap! *statement-selection-listeners* conj {:listener l :args args}))
  
  (register-search-listener
   [this l args]
   (register-search-button-listener l args))

  (register-add-existing-premise-listener
   [this l args]
   (swap! *add-existing-premise-listeners* conj {:listener l :args args}))
  
  (register-argument-selection-listener
   [this l args]
   (swap! *argument-selection-listeners* conj {:listener l :args args}))

  (register-premise-selection-listener
   [this l args]
   (swap! *premise-selection-listeners* conj {:listener l :args args}))

  (hide
   [this]
   (.setVisible *frame* true)
   (.dispose *frame*))
  )
