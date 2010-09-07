;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.utils.swing
  (:import java.beans.PropertyChangeListener
           (javax.swing.event ListSelectionListener
                              TreeSelectionListener)
           (javax.swing UIManager
                        JFileChooser
                        filechooser.FileFilter)
           (javax.swing.tree.TreePath)
           (java.awt.event WindowAdapter
                           MouseAdapter)))

(defn create-file-filter [description extensions]
  (letfn [(get-extension [#^String filename]
                         (last (.split filename "\\.")))]
    (proxy [FileFilter] []
      (getDescription []
                      description)
      (accept [#^java.io.File f]
              (or (.isDirectory f)
                  (contains? extensions (get-extension (.getName f))))))))

(defmacro with-tree [tree & body]
  "works on a JTree and restores its expanded paths after executing body"
  `(let [root# (.getRoot (.getModel ~tree))
         expanded# (if-let [x# (.getExpandedDescendants
                                ~tree (TreePath. root#))]
                     (enumeration-seq x#)
                     ())]
     ~@body
     (doseq [path# expanded#]
       (.expandPath ~tree path#))))

(defn add-propertychange-listener
  [component f & args]
  (let [listener (proxy [PropertyChangeListener] []
                   (propertyChange [event]
                                   (apply f event args)))]
    (.addPropertyChangeListener component listener)
    listener))

(defn add-listselection-listener
  [component f & args]
  (let [listener (proxy [ListSelectionListener] []
                   (valueChanged [event]
                                 (apply f event args)))]
    (.addListSelectionListener component listener)
    listener))

(defn add-treeselection-listener
  [component f & args]
  (let [listener (proxy [TreeSelectionListener] []
                   (valueChanged [event]
                                 (apply f event args)))]
    (.addTreeSelectionListener component listener)
    listener))

(defn add-mousepressed-listener
  [component f & args]
  (let [listener (proxy [MouseAdapter] []
                     (mousePressed [event]
                                   (apply f event args)))]
    (.addMouseListener component listener)
    listener))

(defn add-mousereleased-listener
  [component f & args]
  (let [listener (proxy [MouseAdapter] []
                     (mouseReleased [event]
                                    (apply f event args)))]
    (.addMouseListener component listener)
    listener))

(defn add-windowclose-listener [component f & args]
  (let [listener (proxy [WindowAdapter] []
                   (windowClosing [event]
                                  (apply f event args)))]
    (.addWindowListener component listener)
    listener))

(defn remove-window-listeners [component]
  (let [listeners (.getWindowListeners component)]
    (doseq [listener listeners]
      (.removeWindowListener component listener))))

(defn remove-action-listeners [component]
  (let [listeners (.getActionListeners component)]
    (doseq [listener listeners]
      (.removeActionListener component listener))))

(defn remove-listselection-listeners [component]
  (let [listeners (.getListSelectionListeners component)]
    (doseq [listener listeners]
      (.removeListSelectionListener component listener))))

(defn remove-mouse-listeners [component]
  (let [listeners (.getMouseListeners component)]
    (doseq [listener listeners]
      (.removeMouseListener component listener))))

(defn set-native-look-and-feel []
  (try
    (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
   (catch Exception e)))

(defn set-look-and-feel [name]
  (try
    (loop [infos (UIManager/getInstalledLookAndFeels)]
      (let [info (first infos)]
        (when-not (empty? infos)
          (if (= name (.getName info))
            (UIManager/setLookAndFeel (.getClassName info))
            (recur (rest infos))))))
    (catch Exception e (prn "Exception") (prn e))))