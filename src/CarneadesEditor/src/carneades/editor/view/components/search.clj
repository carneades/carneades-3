;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.components.search
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.utils.swing
        carneades.editor.view.swinguiprotocol
        [clojure.string :only (trim split)]
        carneades.editor.utils.listeners)
  (:import (java.awt.event KeyEvent KeyAdapter)
           (javax.swing JScrollPane table.DefaultTableModel KeyStroke JTable)
           (carneades.editor.uicomponents EditorApplicationView SearchOptionsDialog)
           (carneades.editor.view.swinguiprotocol StatementInfo ArgumentInfo)))

(defvar- *frame* (EditorApplicationView/instance))

(defvar- *searchButton* (.searchButton *frame*))
(defvar- *searchComboBox* (.searchComboBox *frame*))

(defvar- *optionsButton* (.optionsButton *frame*))
(defvar- *searchPanel* (.searchPanel *frame*))
(defvar- *searchScrollPane* (.searchScrollPane *frame*))
(defvar- *searchProgressBar* (.searchProgressBar *frame*))

(defvar *searchResultTable* (.searchResultTable *frame*))
(defvar- *modelTable* (proxy [DefaultTableModel] []
                        (isCellEditable
                         [r c]
                         false)))

(defvar- *searchActiveMessage* "Stop search")
(defvar- *searchInactiveMessage* "Search")

(defvar- *searchActiveMessage* "Stop search")
(defvar- *searchInactiveMessage* "Search")

(defvar- *state* (atom false))
(defvar- *searchactive* (atom false))

(gen-listeners-fns "searchresult-selection")
(gen-listeners-fns "keyenter-searchresult")
(gen-listeners-fns "search-button")

(defn- update-scrollbar-size []
  ;; see http://forums.sun.com/thread.jspa?threadID=5426991
  (.setPreferredSize *searchPanel* nil)
  (.setPreferredSize *searchScrollPane* nil)
  (let [scrollbar (.getVerticalScrollBar *searchScrollPane*)
        dimscrollbar (.getPreferredSize scrollbar)
        dim (.getPreferredSize *searchPanel*)]
    (.setSize dim (.width dimscrollbar) (.height dim))
    (.setVisible scrollbar true)
    (.setPreferredSize scrollbar dim)
    (.setSize scrollbar dim)))

(defvar- *search-options* (atom {:search-for-statements true
                                 :search-for-arguments true
                                 :search-in :current-graph}))

(defn- showoptions-button-listener [event]
  (let [dialog (SearchOptionsDialog. *frame* true)
        cancelbutton (.cancelbutton dialog)
        okbutton (.okbutton dialog)
        searchincurrentgraph (.searchInCurrentGraphButton dialog)
        searchalllkiffiles (.searchInAllLkifFilesButton dialog)
        searchforstatements (.searchForStatementsCheckBox dialog)
        searchforarguments (.searchForArgumentsCheckBox dialog)
        {:keys [search-in search-for-arguments search-for-statements]}
        (deref *search-options*)]
    (add-action-listener cancelbutton (fn [event] (.dispose dialog)))
    (add-action-listener okbutton
                         (fn [event]
                           (swap! *search-options* assoc
                                  :search-in (if (.isSelected searchincurrentgraph)
                                               :current-graph
                                               :all-lkif-files)
                                  :search-for-statements (.isSelected searchforstatements)
                                  :search-for-arguments (.isSelected searchforarguments))
                           (.dispose dialog)))
    (.setSelected searchincurrentgraph (= search-in :current-graph))
    (.setSelected searchalllkiffiles (= search-in :all-lkif-files))
    (.setSelected searchforstatements search-for-statements)
    (.setSelected searchforarguments search-for-arguments)
    (.setLocationRelativeTo dialog *frame*)
    (.setVisible dialog true)))

(defn- add-item-to-search-box [item]
  (loop [n (dec (.getItemCount *searchComboBox*))]
    (cond (neg? n)
          (.addItem *searchComboBox* item)
          
          (not= (.getItemAt *searchComboBox* n) item)
          (recur (dec n)))))

(defn- get-searched-info []
  (prn "get-searched-info")
  (prn (deref *search-options*))
  (let [text (.getSelectedItem *searchComboBox*)]
    (if (nil? text)
     nil
     [(trim text) (deref *search-options*)])))

(defn set-search-state [active]
  (reset! *searchactive* active)
  (if active
    (do
      (loop [rowcount (.getRowCount *modelTable*)]
        (when (pos? rowcount)
          (.removeRow *modelTable* 0)
          (recur (.getRowCount *modelTable*))))
      (.setText *searchButton* *searchActiveMessage*)
      (.setIndeterminate *searchProgressBar* true))
    (do
      (.setText *searchButton* *searchInactiveMessage*)
      (.setIndeterminate *searchProgressBar* false))))

(defn- search-button-listener [event]
  (prn "event")
  (prn event)
  (Thread/sleep 50)
  (let [was-active (deref *searchactive*)]
    (when-not was-active
      (when-let [text (.getSelectedItem *searchComboBox*)]
        (add-item-to-search-box (trim text))))
    (if was-active
      (call-search-button-listeners false nil)
      (call-search-button-listeners true (get-searched-info)))))

(defn selected-object-in-search-result []
  (when-let [row (first (.getSelectedRows *searchResultTable*))]
    (.getValueAt *modelTable* row 0)))

(defn search-result-selection-listener [event]
  (call-searchresult-selection-listeners event))

(defn- create-search-result-keylistener []
  (proxy [KeyAdapter] []
    (keyPressed
     [keyevent]
     (when (= (.getKeyCode keyevent) KeyEvent/VK_ENTER)
       (call-keyenter-searchresult-listeners keyevent)
       (.consume keyevent)))))

(defn init-search []
  (doseq [col ["Results"]]
    (.addColumn *modelTable* col))
  (.setModel *searchResultTable* *modelTable*)
    ;; map enter key to edit
  (.addKeyListener *searchResultTable* (create-search-result-keylistener))
  
  ;;  This prevents action events from being fired when the
  ;;  up/down arrow keys are used on the dropdown menu
  (.putClientProperty *searchComboBox* "JComboBox.isTableCellEditor" true)
  (add-listselection-listener (.getSelectionModel *searchResultTable*)
                              search-result-selection-listener)
  (add-action-listener
   *searchComboBox*
   (fn [event]
     (when (and (= (.getActionCommand event)
                   "comboBoxEdited")
                ;; prevent:
                ;; http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6336981
                (.isFocusOwner
                 (.. *searchComboBox* getEditor getEditorComponent)))
       (search-button-listener event))))
  ;; (set-options-visible (deref *state*))
  (add-action-listener *optionsButton* showoptions-button-listener)
  (add-action-listener *searchButton* search-button-listener))

(defn add-stmt-search-result [path id stmt stmt-fmt]
  (let [obj (StatementInfo. path id stmt stmt-fmt)]
    (.insertRow *modelTable* (.getRowCount *modelTable*)
                (to-array [obj]))))

(defn add-arg-search-result [path id arg title]
  (let [obj (ArgumentInfo. path id arg title)]
    (.insertRow *modelTable* (.getRowCount *modelTable*)
                (to-array [obj]))))

