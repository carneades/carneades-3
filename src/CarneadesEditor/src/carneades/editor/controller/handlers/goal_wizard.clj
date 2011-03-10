;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Implementation of the Find Positions assistant."}
  carneades.editor.controller.handlers.goal-wizard
  (:use clojure.contrib.def
        clojure.contrib.pprint
        clojure.contrib.swing-utils
        ;; carneades.engine.utils
        carneades.editor.utils.core
        carneades.editor.view.viewprotocol
        carneades.editor.view.wizardsprotocol
        carneades.editor.view.swinguiprotocol
        carneades.editor.controller.handlers.messages
        carneades.editor.controller.documents
        [carneades.engine.statement :only (statement-formatted statement-complement
                                                               statement-atom)]
        carneades.engine.position-sort
        [carneades.engine.abduction :only (*verum-clause*
                                           statement-in-label
                                           statement-out-label
                                           assume-decided-statements
                                           make-minimal)])
  (:import carneades.editor.view.wizardsprotocol.StatementItem))

(defn on-pre-goalwizard [view path id]
  (when-let [ag (get-ag path id)]
    (let [mainissue (statement-formatted (:main-issue ag))]
      (if (empty? mainissue)
        (do
          (display-error view *goalwizard-error* *no-mainissue*)
          false)
        (do
          (set-main-issue view mainissue)
          true)))))

(defvar- *positions* (atom nil))
(defvar- *abduction-state* (atom :stopped) ":stopped, :running or :stopping") 
(defvar- *abduction-future* (atom nil))

(defn on-abduction-panel-validation [settings view path id]
  (let [state (deref *abduction-state*)]
    (cond (or (= state :running) (= state :stopping))
          *searching-positions*

          (nil? (deref *positions*)) nil

          :else (let [statement (get settings "statements")]
                  (if (nil? statement)
                    *select-statement*
                    nil)))))

(defn- run-abduction [settings view path id]
  (m-let [ag (get-ag path id)
          positive (get settings "positive")
          negative (get settings "negative")
          in (get settings "in")
          out (get settings "out")
          mainissue (:main-issue ag)
          positions (cond (and positive in)
                          (statement-in-label ag (assume-decided-statements ag)
                                              mainissue)

                          (and positive out)
                          (statement-out-label ag (assume-decided-statements ag)
                                               mainissue)

                          (and negative in)
                          (statement-in-label ag (assume-decided-statements ag)
                                              (statement-complement mainissue))

                          (and negative out)
                          (statement-out-label ag (assume-decided-statements ag)
                                               (statement-complement mainissue)))
          positions  (apply vector (sort-by count positions))
          position (first positions)
          npos (count positions)]
    (when (= (deref *abduction-state*) :running)
      (if (= positions [*verum-clause*])
        (do
          (do-swing-and-wait
           (reset! *abduction-state* :stopped)
           (reset! *positions* nil)
           ;; force an event so that the validator is called again:
           (reset-position view)
           (display-position view nil 0 0 nil)
           (set-abduction-busy view false)))
        (do
          (reset! *positions* {:positions positions :index 0 :npos npos
                               :posnotminimized positions})
          (do-swing-and-wait
           (reset! *abduction-state* :stopped)
           (set-abduction-busy view false)
           (display-position view position 0 npos statement-formatted)))))))

(defn- try-stop-abduction []
  (when-let [abduction-future (deref *abduction-future*)]
    (reset! *abduction-state* :stopping)
    (future-cancel abduction-future)
    (when (future-cancelled? abduction-future)
      (reset! *abduction-state* :stopped))))

(defn on-abduction-panel [settings view path id]
  (locking *abduction-state*
    (reset-position view)
    (letfn [(start-abduction
             []
             (set-abduction-busy view true)
             (reset! *abduction-state* :running)
             (reset! *abduction-future*
                     (future (run-abduction settings view path id))))
            
            (wait-for-abduction
             []
             (deref *abduction-future*)
             (reset! *abduction-state* :stopped))

            (wait-then-start-abduction
             []
             (wait-for-abduction)
             (start-abduction))]
      (condp = (deref *abduction-state*)
          :running (do
                     (try-stop-abduction)
                     (when (not= (deref *abduction-state*) :stopped)
                       (wait-for-abduction))
                     (start-abduction))
          
          :stopped (start-abduction)

          :stopping (wait-then-start-abduction)))))

(defn on-post-goalwizard [view path id settings]
  (when settings
    (m-let [ag (get-ag path id)
            statement (:stmt (get settings "statements"))]
      (display-statement view path ag (statement-atom statement) statement-formatted))))

(defn on-first-position [view path id]
  (let [positions (deref *positions*)]
    (when (and (future-done? (deref *abduction-future*)) (not (nil? positions)))
      (let [{:keys [positions npos]} positions
            index 0
            position (get positions index)]
        (swap! *positions* assoc :index index)
        (display-position view position index npos statement-formatted)))))

(defn on-previous-position [view path id]
  (let [positions (deref *positions*)]
    (when (and (future-done? (deref *abduction-future*)) (not (nil? positions)))
      (let [{:keys [index positions npos]} positions]
        (when (pos? index)
          (let [index (dec index)
                position (get positions index)]
            (swap! *positions* assoc :index index)
            (display-position view position index npos statement-formatted)))))))

(defn on-next-position [view path id]
  (let [positions (deref *positions*)]
    (when (and (future-done? (deref *abduction-future*)) (not (nil? positions)))
      (let [{:keys [index positions npos]} positions]
        (when (< index (dec npos))
          (let [index (inc index)
                position (get positions index)]
            (swap! *positions* assoc :index index)
            (display-position view position index npos statement-formatted)))))))

(defn on-last-position [view path id]
  (let [positions (deref *positions*)]
    (when (and (future-done? (deref *abduction-future*)) (not (nil? positions)))
      (let [{:keys [index positions npos]} positions]
        (let [index (dec npos)
              position (get positions index)]
          (swap! *positions* assoc :index index)
          (display-position view position index npos statement-formatted))))))

(defn on-sort-by [view path id sort-by-val]
  (m-let [positions (deref *positions*)
          npos (:npos positions)
          positions (:positions positions)
          ag (get-ag path id)
          positions (apply vector
                           (condp = sort-by-val
                                 "smaller first" (sort-by count positions)
                                 "closer to issue first" (sort-by #(position-depth ag %) positions)
                                 "farther from issue first" (sort-by #(position-height ag %) positions)))
          position (first positions)]
    (swap! *positions* assoc :positions positions :index 0)
    (display-position view position 0 npos statement-formatted)))

(defn on-minimize-positions [view path id minimize]
  (m-let [positions (deref *positions*)
          positions (if minimize
                      (apply vector (make-minimal (:positions positions)))
                      (:posnotminimized positions))
          npos (count positions)]
    (swap! *positions* assoc :positions positions :npos npos :index 0)
    (display-position view (first positions) 0 npos statement-formatted)))

(defn on-cancel-goal-wizard [settings view path id]
  (try-stop-abduction)
  true)
