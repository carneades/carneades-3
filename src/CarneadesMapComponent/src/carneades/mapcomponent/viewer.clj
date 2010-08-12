;;; Carneades Argumentation Library and Tools.
;;; Copyright (C) 2010 Thomas F. Gordon, Fraunhofer FOKUS, Berlin
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License version 3 (LGPL-3)
;;; as published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ns carneades.mapcomponent.viewer
  (:use clojure.contrib.def
        clojure.contrib.pprint
        [carneades.engine.statement :only (statement-formatted)]
        carneades.mapcomponent.map)
  (:import (javax.swing JFrame JMenu JFileChooser JMenuBar JMenuItem
                        JWindow SwingConstants UIManager filechooser.FileFilter)
           (java.awt.event KeyEvent ActionListener)))

(defvar- *title* "Carneades")

;; code to create the menu
(defn- create-file-filter []
  (letfn [(extension [#^String filename]
                     (last (.split filename "\\.")))]
    (proxy [FileFilter] []
      (getDescription []
                      "SVG Files")
      (accept [#^java.io.File f]
              (or (.isDirectory f)
                  (= "svg" (extension (.getName f))))))))

(defn- on-export-as-svg [#^JFrame frame]
  (let [component (.. frame getContentPane (getComponent 0))
        filechooser (JFileChooser.)]
    (doto filechooser
      (.setDialogTitle "Export as SVG")
      (.setFileFilter (create-file-filter))
      (.showSaveDialog frame))
    (if-let [file (.getSelectedFile filechooser)]
      (export-graph component (.getPath file)))))

(defn- on-exit-item [#^JFrame frame]
  (doto frame
    (.setVisible false)
    (.dispose)))

(defn- create-menubar [frame]
  (let [menuBar (JMenuBar.)
        fileMenu (JMenu. "File")
        exportItem (JMenuItem. "Export as SVG..." KeyEvent/VK_E)
        exitItem (JMenuItem. "Exit" KeyEvent/VK_X)]
    (.setMnemonic fileMenu KeyEvent/VK_F)
    (.addActionListener exitItem
                        (proxy [ActionListener] []
                          (actionPerformed [e]
                                           (on-exit-item frame))))
    (.addActionListener exportItem
                        (proxy [ActionListener] []
                          (actionPerformed [e]
                                           (on-export-as-svg frame))))
    (.add fileMenu exportItem)
    (.insertSeparator fileMenu 1)
    (.add fileMenu exitItem)
    (.add menuBar fileMenu)
    menuBar))

(defn set-look-and-feel [name]
  (try
    (loop [infos (UIManager/getInstalledLookAndFeels)]
      (let [info (first infos)]
        (when-not (empty? infos)
          (if (= name (.getName info))
            (UIManager/setLookAndFeel (.getClassName info))
            (recur (rest infos))))))
    (catch Exception e (prn "Exception") (prn e))))

(defn view-graph [ag stmt-str]
  (set-look-and-feel "Nimbus")
  (let [frame (JFrame. *title*)]
    (doto frame
      (.setJMenuBar (create-menubar frame))
      (.. getContentPane (add (create-graph-component ag stmt-str)))
      ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setSize 800 800)
      (.setVisible true))))

;; (defmethod view-graph "jgraph" [viewer ag stmt-str]
;;   (view-jgraph ag stmt-str))
(defn view [ag]
  (view-graph ag statement-formatted))
