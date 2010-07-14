(ns carneades.editor.view.editorapplication
  (:use clojure.contrib.def
        carneades.mapcomponent.map)
  (:import java.awt.EventQueue
           (javax.swing UIManager JFrame JInternalFrame JFileChooser
                        filechooser.FileFilter)
           carneades.editor.uicomponents.EditorApplicationView))

(defprotocol View
  (display-graph [this ag stmt-fmt] "take the graph and a function that format 
                                     a statement")

  (ask-lkif-file-to-open [this] "ask the user the LKIF file to open. 
                                 Returns File or nil")

  (display-graphs-list [this graphsid] "display a list of graphs that can be 
                                        displayed")
  )


(defvar- *frame* EditorApplicationView/instance)
(defvar- *mapPanel* EditorApplicationView/mapPanel)
(defvar- *argumentgraphsList* EditorApplicationView/argumentgraphsList)


(defn- set-native-look-and-feel []
  (try
   (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
   (catch Exception e)))

(defn launch-application [& args]
  (set-native-look-and-feel)
  (.setDefaultCloseOperation *frame* JFrame/DISPOSE_ON_CLOSE)
  (EventQueue/invokeLater
   (proxy [Runnable] []
     (run []
          (.setVisible *frame* true)))))

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
  (display-graph
   [this ag stmt-fmt]
   (let [component (create-graph-component ag stmt-fmt)]
     (.addTab *mapPanel* (format "%s:%s" (:id ag) (:title ag))
              nil component nil)))
  
  (ask-lkif-file-to-open
   [this]
   (let [jc (JFileChooser.)]
     (.setFileFilter jc (create-file-filter))
     (let [val (.showOpenDialog jc *frame*)]
       (when (= val JFileChooser/APPROVE_OPTION)
         (.getSelectedFile jc)))))

  (display-graphs-list
   [this graphsid]
   (prn "set list data")
   (prn graphsid)
   (.setListData *argumentgraphsList* (to-array graphsid))))

