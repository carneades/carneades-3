(ns carneades.editor.utils.swing
  (:import (javax.swing.event ListSelectionListener
                              TreeSelectionListener)
           javax.swing.UIManager
           (javax.swing.tree.TreePath)
           (java.awt.event WindowAdapter
                           MouseAdapter)))

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
    (doseq [info (UIManager/getInstalledLookAndFeels)]
      (when (= name (.getName info))
        (UIManager/setLookAndFeel (.getClassName info))))
    (catch Exception e)))
