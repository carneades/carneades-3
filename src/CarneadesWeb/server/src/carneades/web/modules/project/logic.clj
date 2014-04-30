;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.modules.project.logic
  ^{:doc "Basic functions for serving project requests"}
  (:require [clojure.string :refer [join]]
            [clojure.set :as set]
            [clojure.zip :as z]
            [clj-http.client :as client]
            [taoensso.timbre :as timbre :refer [trace debug info warn error fatal spy]]
            [compojure.route :as route]
            [cheshire.core :refer :all]
            [carneades.maps.lacij :as lacij]
            [carneades.database.db :as db]
            [carneades.database.export :refer [export-to-argument-graph]]
            [carneades.engine.theory :as t]
            [carneades.engine.theory.zip :as tz]
            [carneades.project.admin :as project]
            [carneades.engine.translation :as tr]
            [carneades.engine.theory.translation :as ttr]
            [clojure.java.io :as io]
            [carneades.engine.utils :refer [dissoc-in unserialize-atom]]
            [carneades.engine.theory :as theory]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- set-lang-description [lang key x] (assoc x key (-> x key lang)))

(defn- filter-refs [x] (not (= (:key x) nil)))

(defn- rename-keys [x] (set/rename-keys x {:creation-date :date}))

(defn- normalize [lang data]
  (reduce (fn [x y] (conj x (->>
                            (#(set-lang-description lang :description %))
                            (#(rename-keys %)))))
          data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn build-url
  [host resource params]
  (let [path (cons (str host "/carneades/carneadesws/" (name resource)) params)
        url (str "http://" (join "/" path))]
    url))

(defn get-resource
  "Returns a JSON resource from the old carneades REST api."
  [host resource params]
  {:pre [(not (nil? resource))]}
  (let [url (build-url host resource params)
        content (:body (client/get url))]
    (parse-string content true)))

(defn get-raw-resource
  [host resource params]
  (io/input-stream (:body (client/get (build-url host resource params)
                                      {:as :byte-array}))))

(defn post-resource
  [host resource params post-params]
  (client/post (build-url host resource params) post-params))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for service calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-node-text [node index]
  (if (contains? node :premises)
    (cond
     (and (contains? node :header)
          (contains? (:header node) :title))
     (:en (:title (:header node)))
     :else (str "Argument #" index))

    (cond
     (contains? node :text)
     (:en (:text node))
     :else (:atom node))))

(defn get-type [node]
  (if (contains? node :premises)
    (if (and (contains? node :pro)
             (= (:pro node) true))
      "pro"
      "con")
    ""))

(defn make-node [nodes lang index]
  ;; TODO: to rewrite properly
  (with-local-vars [idx index, c 1]
    (reduce (fn [x y]
              (var-set idx (+ @idx @c))
              (conj x (-> {}
                          (assoc :id (:id (get y 0)))
                          (assoc :text (str (get-type (get y 0))
                                            " "
                                            (get-node-text (get y 0) @idx)))
                          (assoc :children (make-node (get y 1) lang 0))
                          (assoc :value (:value (get y 0)))
                          )))
            []
            nodes)))

(defn make-issues
  [outline]
  (map #(select-keys % [:id :text :value]) outline))

(defn make-outline
  [project db & {:keys [lang host] :or {lang :en host "localhost:3000"}}]
  (make-node (get (:outline (get-resource host :outline [project db])) 1) lang 0))

(defn get-sub-outline [outline id] outline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of service calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-projects
  [& {:keys [id lang host]}]
  (->> (if (nil? id) [] [id])
       (#(get-resource host :project %))
       (#(if-not (seq? %) (list %) %))
       (#(map (comp (partial set-lang-description lang :description)
                    (partial rename-keys))
              %))
       (#(if (= (count %) 1) (first %) %))))

(defn get-outline
  [[project db id :as params]
   & {:keys [lang host] :or {lang :en k nil host "localhost:3000"}}]
  (info "calling outline")
  (->> (make-outline project db :host host :lang lang)
       (#(if-not (nil? id) (get-sub-outline % id) %))))

(defn get-metadata
  [[project db id :as params] & {:keys [k lang host]}]
  {:pre [(not (nil? project))
         (not (nil? db))]}
  (->> (get-resource host :metadata params)
       (#(if (nil? k)
           %
           (filter (fn [x] (= (:key x) k)) %)))))

(defn get-metadatum
  [[project db id :as params] & {:keys [k lang host]}]
  {:pre [(not (nil? project))
         (not (nil? db))]}
  (->> (get-resource host :metadata params)
       (#(if (nil? k)
           %
           (filter (fn [x] (= (:key x) k)) %)))
        (#(if (contains? % :description)
           (set-lang-description lang :description %)
           %))))

(defn trim-premises
  "Removes premises information that are not used from a collection of premises."
  [premises lang]
  (reduce
   (fn [acc p]
     (conj acc {:id (-> p :statement :id)
                :text (-> p :statement :text lang)
                :role (:role p)
                :positive (:positive p)}))
   []
   premises))

(defn trim-conclusion
  "Removes unused information from the conclusion"
  [conclusion lang]
  (-> conclusion
      (select-keys [:id :positive :text])
      (assoc :text (-> conclusion :text lang))
      ))

(defn trim-metadata
  "Removes unused languages and information from metadata"
  [metadata lang]
  (assoc metadata :description (lang (:description metadata))))

(defn trim-argument
  "Removes unused information from an argument (from the perspective
  of a statement)."
  [argument lang]
  (-> argument
      (select-keys [:scheme :premises :id :conclusion])))

(defn get-theory
  [{:keys [tpid tid scheme lang translate]}]
  (let [theory (assoc (project/load-theory tpid tid) :id tid)
        ;; TODO: creates a simpler function that just returns the :translation key?
        translator (comp (tr/make-default-translator)
                         (tr/variable-converter-translator)
                         (ttr/make-language-translator (:language theory)))
        do-translation (or (= translate "t") (= translate "true"))]
    (cond (and do-translation scheme)
          (when-let [s (t/find-scheme theory (symbol scheme))]
            (ttr/translate-scheme s translator lang))

          scheme
          (t/find-scheme theory (symbol scheme))

          do-translation
          (-> theory
              (ttr/translate-theory translator lang)
              (dissoc :language))

          :else
          (dissoc theory :language))))

(defn get-theories
  [params]
  (let [params (merge {;; :host "localhost:3000"
                       :lang :en} params)
        params (update-in params [:lang] keyword)]
    (if (:tid params)
      (get-theory params)
      (map #(get-theory (assoc params :tid %))
           (:theories (get-resource (:host params) :project [(:pid params) "theories"]))))))

(defn trim-scheme
  [scheme]
  (select-keys scheme [:id :header]))

(defn get-scheme-from-arg
  [project arg host lang]
  (let [pcontent (get-resource host :project [project])
        schemestr (str (first (unserialize-atom (:scheme arg))))
        schemes-project (theory/get-schemes-project project (:schemes pcontent))
        schemes-name (theory/get-schemes-name (:schemes pcontent))
        scheme (get-theory {:pid schemes-project :tid schemes-name :scheme schemestr :lang lang})]
    (if (nil? scheme)
      ;; no scheme found? fake one
      {:header {:title schemestr} :id schemestr}
      scheme)))

(defn get-argument
  [[project db id :as params]
   & {:keys [host lang] :or {host "localhost:3000" lang :en}}]
  (debug "get-argument")
  {:pre [(not (nil? project))
         (not (nil? db))]}
  (let [arg (get-resource host :argument [project db id])
        scheme (get-scheme-from-arg project arg host lang)]
    (-> arg
        (update-in [:header] trim-metadata lang)
        (update-in [:conclusion] trim-conclusion lang)
        (update-in [:premises] trim-premises lang)
        (assoc :scheme (trim-scheme scheme)))))

(defn get-trimed-argument
  [project db host lang aid]
  (let [arg (get-argument [project db aid] :host host :lang lang)]
   (trim-argument arg lang)))

(defn get-statement
  [[project db id :as params]
   & {:keys [lang host] :or {lang :en host "localhost:3000"}}]
  {:pre [(not (nil? project))
         (not (nil? db))]}
  (let [stmt (get-resource host :statement params)
        stmt (update-in stmt [:header] trim-metadata lang)
        stmt (assoc stmt :text (lang (:text stmt)))
        stmt (assoc stmt :pro (map (partial get-trimed-argument project db host lang)
                                   (:pro stmt)))
        stmt (assoc stmt :con (map (partial get-trimed-argument project db host lang)
                                   (:con stmt)))
        stmt (assoc stmt :premise-of
                    (map (partial get-trimed-argument project db host lang) (:premise-of stmt)))]
    stmt))

(defn get-nodes
  [project db id & {:keys [lang host] :or {lang :en host "localhost:3000"}}]
  (let [[info outline refs]
        [(get-metadata [project db id] :host host :lang lang)
         (get-outline [project db] :host host :lang lang)
         (get-metadata [project db] :host host :lang lang)]]
    (-> {}
        (assoc :description (-> info :description lang))
        (assoc :issues (make-issues outline))
        (assoc :outline outline)
        (assoc :references (map #(select-keys % [:creator :date :identifier :title])
                                (filter filter-refs refs))))))

(defn get-issues
  [[project db :as params] & {:keys [lang host]}]
  (let [outline (get-outline [project db] :host host :lang lang)]
    (make-issues outline)))

(defn get-references
  [[project db :as params] & {:keys [lang host]}]
  (let [references (get-metadata [project db] :host host :lang lang)]
    (map #(select-keys % [:creator :date :identifier :title :key])
         (filter filter-refs references))))

(defn get-argument-map [project db & {:keys [lang host] :or {lang :en host "localhost:3000"}}]
  (let [options {:db db :lang lang}
        dbcon (db/make-connection project db "guest" "")]
    (db/with-db dbcon
      (let [convert-option (fn [val]
                             (try
                               (Integer/parseInt val)
                               (catch Exception _
                                 (keyword val))))
            ag (export-to-argument-graph dbcon)
            optionsseq (mapcat (fn [[k v]] [k (convert-option v)]) options)
            svg (apply lacij/export-str ag lang optionsseq)]
        svg))))

(defn get-project-archive
  [& {:keys [project host]}]
  (get-raw-resource host :export [(str project ".zip")]))

(defn post-project-archive
  [& {:keys [file host]}]
  ;; curl -F "file=@default2.zip;filename=nameinpost" http://localhost:3000/api/projects/upload
  (post-resource host :import []
                 {:multipart
                  [{:name "Content/type" :content "application/zip"}
                   {:name "file"
                    :content (clojure.java.io/file (.getPath (:tempfile file)))}]}))

(defn get-theme
  [[project did :as params] & {:keys [host]}]
  (get-raw-resource host :theme [project did]))
