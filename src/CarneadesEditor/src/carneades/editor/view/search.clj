;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.search
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.utils.swing
        carneades.editor.view.swinguiprotocol
        [clojure.string :only (trim split)])
  (:import (java.awt.event KeyEvent KeyAdapter)
           (javax.swing JScrollPane table.DefaultTableModel KeyStroke JTable)
           (carneades.editor.uicomponents EditorApplicationView SearchOptionsDialog)
           (carneades.editor.view.swinguiprotocol StatementInfo)))

(defvar- *frame* (EditorApplicationView/instance))

(defvar- *searchButton* (.searchButton *frame*))
(defvar- *searchComboBox* (.searchComboBox *frame*))

;; (defvar- *optionsPanel* (.optionsPanel *frame*))
(defvar- *optionsButton* (.optionsButton *frame*))
(defvar- *searchPanel* (.searchPanel *frame*))
(defvar- *searchScrollPane* (.searchScrollPane *frame*))
(defvar- *searchProgressBar* (.searchProgressBar *frame*))
;; (defvar- *searchInCurrentGraph* (.searchInCurrentGraphButton *frame*))

(defvar *searchResultTable* (.searchResultTable *frame*))
;; (defvar- *modelTable* (.getModel *searchResultTable*))
(defvar- *modelTable* (proxy [DefaultTableModel] []
                        (isCellEditable
                         [r c]
                         false)))

;; (defvar- *showOptionsMessage* "Show options")
;; (defvar- *hideOptionsMessage* "Hide options")

(defvar- *searchActiveMessage* "Stop search")
(defvar- *searchInactiveMessage* "Search")

(defvar- *searchActiveMessage* "Stop search")
(defvar- *searchInactiveMessage* "Search")

(defvar- *state* (atom false))
(defvar- *searchactive* (atom false))
(defvar- *searchresult-selection-listeners* (atom ()))

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

(defvar- *search-button-listeners* (atom ()))

(defvar- *search-options* (atom {:search-in :current-graph}))

(defn- showoptions-button-listener [event]
  (let [dialog (SearchOptionsDialog. *frame* true)
        cancelbutton (.cancelbutton dialog)
        okbutton (.okbutton dialog)
        searchincurrentgraph (.searchInCurrentGraphButton dialog)
        searchalllkiffiles (.searchInAllLkifFilesButton dialog)
        {:keys [search-in]} (deref *search-options*)]
    (add-action-listener cancelbutton (fn [event] (.dispose dialog)))
    (add-action-listener okbutton
                         (fn [event]
                           (swap! *search-options* assoc
                                  :search-in (if (.isSelected searchincurrentgraph)
                                               :current-graph
                                               :all-lkif-files))
                           (.dispose dialog)))
    (.setSelected searchincurrentgraph (= search-in :current-graph))
    (.setSelected searchalllkiffiles (= search-in :all-lkif-files))
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
    (doseq [{:keys [listener args]} (deref *search-button-listeners*)]
      (if was-active
        (apply listener false nil args)
        (apply listener true (get-searched-info) args)))))

(defn selected-object-in-search-result []
  (when-let [row (first (.getSelectedRows *searchResultTable*))]
    (.getValueAt *modelTable* row 0)))

(defn search-result-selection-listener [event]
  (doseq [{:keys [listener args]} (deref *searchresult-selection-listeners*)]
    (apply listener event args)))

(defvar- *keyenter-searchresult-listeners* (atom ()))

(defn register-keyenter-searchresult-listener [f args]
  (swap! *keyenter-searchresult-listeners* conj {:listener f :args args}))

(defn- create-search-result-keylistener []
  (proxy [KeyAdapter] []
    (keyPressed
     [keyevent]
     (when (= (.getKeyCode keyevent) KeyEvent/VK_ENTER)
       (doseq [{:keys [listener args]} (deref *keyenter-searchresult-listeners*)]
         (apply listener keyevent args)
         (.consume keyevent))))))

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

(defn register-search-button-listener [f args]
  (swap! *search-button-listeners* conj {:listener f :args args}))

(defn file-from-path [path]
  (last (split path (re-pattern java.io.File/pathSeparator))))

(defn add-stmt-search-result [path id stmt stmt-fmt]
  (let [obj (StatementInfo. path id stmt stmt-fmt)]
    (.insertRow *modelTable* (.getRowCount *modelTable*)
                (to-array [obj]))))

(defn register-searchresult-selection-listener [l args]
  (swap! *searchresult-selection-listeners* conj {:listener l :args args}))
