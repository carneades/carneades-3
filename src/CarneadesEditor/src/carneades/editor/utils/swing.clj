;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.utils.swing
  (:require [clojure.zip :as zip])
  (:import java.beans.PropertyChangeListener
           (java.awt Toolkit EventQueue)
           (javax.swing.event ListSelectionListener
                              TreeSelectionListener
                              ChangeListener)
           (javax.swing UIManager
                        JFileChooser
                        filechooser.FileFilter)
           (javax.swing.tree DefaultMutableTreeNode
                             TreePath
                             TreeSelectionModel
                             DefaultTreeModel)
           (java.awt.event WindowAdapter
                           KeyAdapter
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

(defn add-propertychange-listener
  [component f & args]
  (let [listener (proxy [PropertyChangeListener] []
                   (propertyChange [event]
                                   (apply f event args)))]
    (.addPropertyChangeListener component listener)
    listener))

(defn add-change-listener
  [component f & args]
  (let [listener (proxy [ChangeListener] []
                   (stateChanged [event]
                                 (apply f event args)))]
    (.addChangeListener component listener)
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

(defn enable-items [ & items]
  (doseq [item items]
    (.setEnabled item true)))

(defn disable-items [ & items]
  (doseq [item items]
    (.setEnabled item false)))

(defn set-swing-exception-handler [f]
  "forces Swing to calls the function f when an uncaught exception occurred.
   f takes one argument, the uncaught exception"
  ;; http://ruben42.wordpress.com/2009/03/30/catching-all-runtime-exceptions-in-swing/
  (let [toolkit (Toolkit/getDefaultToolkit)
        queue (.getSystemEventQueue toolkit)]
    (.push queue (proxy [EventQueue] []
                   (dispatchEvent
                    [event]
                    (try
                      (proxy-super dispatchEvent event)
                      (catch Throwable e
                        (f e))))))))

(defn add-key-released-listener [component f & args]
  (.addKeyListener component
                   (proxy [KeyAdapter] []
                     (keyReleased
                      [keyevent]
                      (apply f keyevent args)))))

(defn remove-key-listeners [component]
  (doseq [l (.getKeyListeners component)]
    (.removeKeyListener component l)))