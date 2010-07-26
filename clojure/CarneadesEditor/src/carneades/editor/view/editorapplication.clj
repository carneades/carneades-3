(ns carneades.editor.view.editorapplication
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        ;;
        ;; no imports of carneades.engine.* are allowed here
        ;;
        carneades.editor.view.menu.mainmenu
        carneades.mapcomponent.map
        carneades.editor.view.tabs
        carneades.editor.view.tree
        carneades.editor.view.properties.lkif
        carneades.editor.view.properties.properties
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
  (init [this])
  
  (display-error [this title content])
  (show [this] "display the main view, take the command lines arguments
                       as second argument")
  
  (open-graph [this path ag stmt-fmt] "open the graph for edition")

  (close-graph [this path id])

  (current-graph [this] "returns [path id] for the graph currently edited")

  (ask-lkif-file-to-open [this] "ask the user the LKIF file to open. 
                                 Returns File or nil")

  (display-lkif-content [this file graphids]
                        "display information relative to an LKIF file")

  (hide-lkif-content [this path])
  
  (display-lkif-property [this path]))


(defvar- *frame* (EditorApplicationView/instance))

(defn- create-file-filter []
  (letfn [(extension [#^String filename]
                     (last (.split filename "\\.")))]
    (proxy [FileFilter] []
      (getDescription []
                      "LKIF Files")
      (accept [#^java.io.File f]
              (or (.isDirectory f)
                  (= "xml" (extension (.getName f))))))))

(deftype SwingView [] View
  (init
   [this]
   ;; make a new instance (without listeners!)
   (set-look-and-feel "Nimbus")
   (EditorApplicationView/reset)
   (lkif-properties-init))
  
  (display-error
   [this title content]
   (JOptionPane/showMessageDialog *frame* content title
                                  JOptionPane/ERROR_MESSAGE))
  
  (show
   [this]
   (disable-zoom-buttons)
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
         (let [component (create-graph-component ag stmt-fmt)]
           (add-component component path (:id ag))
           (.add *mapPanel* title component)
           (.setTabComponentAt *mapPanel*
                               (.indexOfComponent *mapPanel* component)
                               (create-tabcomponent title))
           (.setSelectedComponent *mapPanel* component)
           (enable-zoom-buttons))))))

  (close-graph
   [this path id]
   (prn "close graph")
   (let [component (get-component path id)]
     (.remove *mapPanel* component)
     (remove-component component))
   (when (tabs-empty?)
     (disable-zoom-buttons)))

  (current-graph
   [this]
   (prn "current-graph")
   (get-graphinfo (.getSelectedComponent *mapPanel*)))

  (display-lkif-property
   [this path]
   (prn "display-lkif-property")
   (show-properties (get-lkif-properties-panel path)))
  
  (ask-lkif-file-to-open
   [this]
   (let [jc (JFileChooser.)]
     (.setFileFilter jc (create-file-filter))
     (let [val (.showOpenDialog jc *frame*)]
       (when (= val JFileChooser/APPROVE_OPTION)
         (.getSelectedFile jc))))))

