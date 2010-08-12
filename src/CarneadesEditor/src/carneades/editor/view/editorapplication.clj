;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.editorapplication
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        ;;
        ;; no imports of carneades.engine.* are allowed here
        ;;

        ;; <debug>
        ;; carneades.examples.hundeanmeldung
        ;; carneades.engine.shell
        ;; </debug>
        
        carneades.editor.view.menu.mainmenu
        carneades.mapcomponent.map
        carneades.editor.view.tabs
        carneades.editor.view.tree
        carneades.editor.view.properties.lkif
        carneades.editor.view.properties.graph
        carneades.editor.view.properties.properties
        carneades.editor.view.aboutbox
        carneades.editor.view.printpreview.preview
        carneades.editor.utils.swing)
  (:import (java.awt EventQueue event.MouseAdapter Dimension FlowLayout Color)
           (javax.swing UIManager JTabbedPane JLabel JButton JFrame JPanel
                        JToolBar
                        JInternalFrame
                        JOptionPane
                        JFileChooser
                        filechooser.FileFilter
                        SwingUtilities
                        BorderFactory
                        tree.DefaultMutableTreeNode
                        tree.TreeSelectionModel)
           (carneades.editor.view.tree GraphInfo LkifFileInfo)
           (carneades.editor.uicomponents EditorApplicationView)))


(defprotocol View
  (init [this] "init the view")
  (show [this] "display the main view, take the command lines arguments
                       as second argument")
  (open-graph [this path ag stmt-fmt] "open the graph for edition")
  (close-graph [this path id])
  (current-graph [this] "returns [path id] for the graph currently edited")
  (ask-lkif-file-to-open [this] "ask the user the LKIF file to open. 
                                 Returns File or nil")
  (ask-file-to-save [this description extension suggested])
  (export-graph-to-svg [this ag stmt-fmt filename])
  (display-lkif-content [this file graphids]
                        "display information relative to an LKIF file")
  (hide-lkif-content [this path])
  (print-preview [this path ag stmt-fmt])
  (display-lkif-property [this path])
  (display-graph-property [this id title mainissue])
  (display-about [this])
  (ask-confirmation [this title content])
  (display-error [this title content]))


(defvar- *frame* (EditorApplicationView/instance))

(defn- create-file-filter [description extension]
  (letfn [(get-extension [#^String filename]
                     (last (.split filename "\\.")))]
    (proxy [FileFilter] []
      (getDescription []
                      description)
      (accept [#^java.io.File f]
              (or (.isDirectory f)
                  (= extension (get-extension (.getName f))))))))

(defvar- *dialog-current-directory* (atom nil))

(defvar- *application-name* "Carneades Editor")

(deftype SwingView [] View
  (init
   [this]
   (System/setProperty "apple.laf.useScreenMenuBar" "true")
   (System/setProperty "com.apple.mrj.application.apple.menu.about.name"
                       *application-name*)

   ;; make a new instance (without listeners!)
   (set-look-and-feel "Nimbus")
   (EditorApplicationView/reset)
   (lkif-properties-init)
   (graph-properties-init))
  
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
   (init-menu)
   (add-treeselection-listener *lkifsTree* on-tree-selection)
   (.addMouseListener *lkifsTree* (create-tree-mouse-listener))
   (.setRootVisible *lkifsTree* false)
   (.setSelectionMode (.getSelectionModel *lkifsTree*)
                       TreeSelectionModel/SINGLE_TREE_SELECTION)
   (.setShowsRootHandles *lkifsTree* true)
   (disable-file-items)
   (.setDefaultCloseOperation *frame* JFrame/DISPOSE_ON_CLOSE)
   (.setTabLayoutPolicy *mapPanel* JTabbedPane/SCROLL_TAB_LAYOUT)
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
           (add-node-selected-listener component nil)
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

  (print-preview
   [this path ag stmt-fmt]
   (let [component (or (get-component path (:id ag))
                       (create-graph-component ag stmt-fmt))]
     (printpreview component)))

  (display-about
   [this]
   (show-about-box *frame*)))

