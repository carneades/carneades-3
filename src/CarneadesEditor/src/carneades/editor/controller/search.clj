;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.search
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        [carneades.engine.statement :only (statement-formatted)]
        [carneades.engine.shell :only (search-statements search-arguments search-all)]
        carneades.editor.model.docmanager
        (carneades.editor.controller documents)
        carneades.editor.view.viewprotocol))

;;; search in background functions

(defvar *end-search* (atom false))
(defvar- *nb-agents-running* (atom 0))
(defvar- *running-futures* (atom ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- do-one-search-stmt [state view]
  (prn "do one search!")
  (let [{:keys [results path id]} state]
    (loop [res results]
      (let [stmt (first res)]
        (when-not (or (nil? stmt) (deref *end-search*))
          (do-swing
           ;; we are not in the swing thread anymore
           ;; so we need to use the do-swing macro
           (display-statement-search-result view path id stmt
                                            statement-formatted))
          (recur (rest res)))))))

(defn- do-one-search-arg [state view]
  (let [{:keys [results path id]} state]
    (loop [res results]
      (let [arg (first res)]
        (when-not (or (nil? arg) (deref *end-search*))
          (do-swing
           (display-argument-search-result view path id arg (:title arg)))
          (recur (rest res)))))))

(defn- do-one-search-all [state view]
  (prn "do-one-search-all")
  (let [{:keys [results path id]} state]
    (loop [res results]
      (let [[type obj] (first res)]
        (when-not (or (nil? obj) (deref *end-search*))
          (case type
                :stmt (do-swing
                       (display-statement-search-result view path id obj
                                                        statement-formatted))

                :arg (do-swing
                      (display-argument-search-result view path id obj (:title obj))))
          (recur (rest res)))))))

(defn- wait-for-futures []
  (doseq [future (deref *running-futures*)]
    (prn "waiting for one future...")
    (deref future)))

(defn- create-search-future [view path ag id search-for-statements
                             search-for-arguments text]
  (cond (and search-for-statements search-for-arguments)
        (let [res (search-all ag statement-formatted text {})]
          (future (do-one-search-all
                   {:results
                    res
                    :path path
                    :id id}
                   view)))

        search-for-statements
        (let [res (search-statements ag
                                     statement-formatted
                                     text {})]
          (future (do-one-search-stmt
                   {:results
                    res
                    :path path
                    :id id}
                   view)))

        search-for-arguments
        (let [res (search-arguments ag text {})]
          (future (do-one-search-arg
                   {:results
                    res
                    :path path
                    :id id}
                   view)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn do-search [view path id text options]
  (prn "do-search")
  (let [{:keys [search-in search-for-statements search-for-arguments]} options]
    (prn "options =")
    (prn options)
    (prn "text =")
    (prn text)
    (when (and (not (empty? text))
               (or (and path (= search-in :current-graph))
                   (= search-in :all-lkif-files)))
      (prn "Search begins")
      (wait-for-futures)
      (let [path-to-id (if (= search-in :all-lkif-files)
                         (mapcat (fn [path]
                                   (partition 2 (interleave
                                                 (repeat path)
                                                 (get-ags-id path))))
                                 (get-all-sectionskeys *docmanager* []))
                         [[path id]])
            nb-ids (count path-to-id)]
        (do-swing-and-wait
         (display-search-state view true))
        (reset! *end-search* false)
        (reset! *nb-agents-running* nb-ids)
        (let [searchfutures
              (doall
               (map (fn [[path id]]
                       (let [ag (get-ag path id)]
                         (create-search-future view path ag id
                                               search-for-statements
                                               search-for-arguments
                                               text)
                         )) path-to-id))]
          (reset! *running-futures* searchfutures)
          (future (wait-for-futures)
                  (do-swing-and-wait
                   (display-search-state view false))))))))
