;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Implementation of the Find Arguments assistant."}
  carneades.editor.controller.handlers.findarguments-wizard
  (:use clojure.contrib.def
        clojure.contrib.pprint
        clojure.contrib.swing-utils
        carneades.editor.controller.documents
        carneades.editor.controller.handlers.messages
        (carneades.engine shell statement lkif)
        [carneades.engine.search :only (depth-first breadth-first)]
        (carneades.editor.view viewprotocol wizardsprotocol)))

(defvar- *goal* (atom nil))

(defn on-pre-findarguments-wizard [view path id goal]
  (when-let [ag (get-ag path id)]
    (if (nil? goal)
      (do
        (display-error view *findargumentswizard-error* *no-statementselected*)
        false)
      (do
        (reset! *goal* goal)
        (set-goal view (statement-formatted goal))
        true))))

(defvar- *newag* (atom nil))
(defvar- *search-state* (atom :stopped) ":stopped, :running or :stopping")
(defvar- *search-future* (atom nil))

(defn on-searcharguments-panel-validation [settings view path id]
  (let [state (deref *search-state*)]
   (cond (or (= state :running) (= state :stopping))
         *searching-arguments*

         :else nil)))

(defn- run-search [settings view path id]
  (when-let [ag (get-ag path id)]
    (do-swing-and-wait
     (set-argumentsearch-busy view true))
    (letfn [(arguments-found?
             [ag ag2]
             (not= (count (:arguments ag)) (count (:arguments ag2))))]
      (let [complement (get settings "complement")
            goal (deref *goal*)
            goal (if complement (statement-complement goal) goal)
            lkif (get-lkif path)
            max-nodes (get settings "max-nodes")
            max-turns (get settings "max-turns")
            search-strategy (get settings "search-strategy")
            strategy (case search-strategy
                           "Depth first" depth-first
                           "Breadth first" breadth-first)
            solutions (construct-arguments goal max-nodes max-turns strategy ag
                                           (list (generate-arguments-from-lkif lkif)))
            ag2 (assoc (unite-solutions solutions)
                  :id (:id ag)
                  :main-issue (:main-issue ag)
                  :title (:title ag))]
        (when (= (deref *search-state*) :running)
          (let [found (arguments-found? ag ag2)]
            (if found 
              (reset! *newag* ag2)
              (reset! *newag* nil))
            (reset! *search-state* :stopped)
            (do-swing-and-wait
             (set-argumentsearch-busy view false)
             (arguments-found view found))))))))

(defn- try-stop-search []
  (when-let [search-future (deref *search-future*)]
    (reset! *search-state* :stopping)
    (future-cancel search-future)
    (when (future-cancelled? search-future)
      (reset! *search-state* :stopped))))

(defn on-searcharguments-panel [settings view path id]
  (letfn [(start-search
            []
            (reset! *search-state* :running)
            (reset! *search-future*
                    (future (run-search settings view path id))))
          
           (wait-for-search
            []
            (deref *search-future*)
            (reset! *search-state* :stopped))

           (wait-then-start-search
            []
            (wait-for-search)
            (start-search))]
   (case (deref *search-state*)
         :running
         (do (try-stop-search)
             (when (not= (deref *search-state*) :stopped)
               (wait-for-search))
             (start-search))

         :stopped
         (start-search)

         :stopping (wait-then-start-search))))

(defn on-post-findarguments-wizard [view path id settings]
  (when settings
    (when-let [ag (deref *newag*)]
      (do-ag-update view [path :ags (:id ag)] ag)
      (graph-changed view path ag statement-formatted)
      (display-statement view path ag (deref *goal*) statement-formatted))))

(defn on-cancel-findarguments-wizard [settings view path id]
  (try-stop-search)
  true)
