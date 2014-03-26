;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.web.modules.project.functions
  ^{:doc "Basic functions for serving project requests"}
  (:require [clj-http.client :as client]
            [taoensso.timbre :as timbre :refer [trace debug info warn error fatal spy]]
            [compojure.route :as route]
            [clojure.set :as set]
            [clojure.zip :as z]
            [cheshire.core :refer :all]
            [carneades.maps.lacij :as lacij]
            [carneades.database.db :as db]
            [carneades.database.export :refer [export-to-argument-graph]]
            [carneades.engine.theory :as t]
            [carneades.engine.theory.zip :as tz]
            [carneades.project.admin :as project]
            [carneades.engine.translation :as tr]
            [carneades.engine.theory.translation :as ttr]
            [clojure.java.io :as io]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- params->path [params]
  (reduce (fn [x y] (conj x (str "/" y))) [] params))

(defn- params->resource
  [base & [params]]
  (->> (into base params)
        params->path
        (apply str "http:/")
        (#(:body (client/get %)))
        (#(parse-string % true))))

(defn- key-filter [keys x] (select-keys x keys))

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
(defn get-resource
  "Returns a JSON resource from the old carneades REST api."
  [host resource params]
  {:pre [(not (nil? resource))]}
  (params->resource [(str host "/carneades/carneadesws/" (name resource))] params))

(defn construct-url
  [host resource params]
  (let [base ["http://" host "/carneadesws/" (name resource)]]
    (if (empty? params)
      (apply str base)
      (apply str (into base ["/"] params)))))

(defn get-raw-resource
  [host resource params]
  (io/input-stream (:body (client/get (construct-url host resource params)
                                      {:as :byte-array}))))

(defn post-resource
  [host resource params post-params]
  (client/post (construct-url host resource params) post-params))

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
  (with-local-vars [idx index, c 1]
    (reduce (fn [x y]
              (var-set idx (+ @idx @c))
              (conj x (-> {}
                          (assoc :id (:id (get y 0)))
                          (assoc :text (str (get-type (get y 0))
                                            " "
                                            (get-node-text (get y 0) @idx)))
                          (assoc :children (make-node (get y 1) lang 0)))))
            []
            nodes)))

(defn make-issues [outline]
  (reduce (fn [x y] (conj x (-> {}
                               (assoc :id (:id y))
                               (assoc :text (:text y)))))
          []
          outline))

(defn make-outline
  [project db & {:keys [lang host] :or {lang :en host "localhost:3000"}}]
  (make-node (get (:outline (get-resource host :outline [project db])) 1) lang 0))

(defn get-sub-outline [outline id] outline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of service calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-projects
  [& {:keys [id lang host]
      :or {lang :en host "localhost:3000"}}]

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
  (->> (make-outline project db :host host :lang lang)
       (#(if-not (nil? id) (get-sub-outline % id) %))))

(defn get-metadata
  [[project db id :as params] & {:keys [k lang host]}]
  {:pre [(not (nil? project))
         (not (nil? db))]}
  (info (str "METADATA-KEY:" k))
  (->> (get-resource host :metadata params)
       (#(if (nil? k)
           %
           (filter (fn [x] (= (:key x) k)) %)))))

(defn get-metadatum
  [[project db id :as params] & {:keys [k lang host]}]
  {:pre [(not (nil? project))
         (not (nil? db))]}
  (info (str "METADATA-KEY:" k))
  (->> (get-resource host :metadata params)
       (#(if (nil? k)
           %
           (filter (fn [x] (= (:key x) k)) %)))
        (#(if (contains? % :description)
           (set-lang-description lang :description %)
           %))))


(defn filter-by [m k ref]
  (if-not (nil? (k ref)) (assoc m k (k ref)) m))


(defn abc [data lang]
  (reduce
   (fn [x y]
     (conj x {:id (-> y :statement :id)
              :text (-> y :statement :text lang)}))
   []
   (:premises data)))

(defn get-arguments [[project db id :as params]
                     & {:keys [host lang] :or {host "localhost:3000" lang :en}}]

  {:pre [(not (nil? project))
         (not (nil? db))]}
  (->> (get-resource host :argument params)

       (#(-> (select-keys % [:id :scheme :strict :weight :value])
             (assoc :premises (abc % lang))
             (assoc :conclusion (str (get-type %)
                                     " - "
                                     (lang (:text (:conclusion %)))))))))

(defn get-arguments-dbg [[project db id :as params]
                     & {:keys [host] :or {host "localhost:3000"}}]

  {:pre [(not (nil? project))
         (not (nil? db))]}
  (get-resource host :argument params))

(defn arg-id->premises
  [[aids p db :as params]
   & {:keys [host lang] :or {host "localhost:3000" lang :en}}]
  "aids - vector of argument ids each represented as string value
   p    - project id
   db   - database id

   Transforms an argument id into a map. The map looks like:
     {aid premises}, where

   aid is the string representation of the argument id used as key,
   premises is a vector containing the text field data of the argument retrieved
            from (get-arguments [p db aid])"
  (reduce (fn [aggregate id]
            (if-let [arg (get-arguments [p db id] :lang lang :host host)]
              (conj aggregate
                    {(:id arg)
                     (:premises arg)})))
          []
          aids))

(defn get-statements
  [[project db id :as params]
   & {:keys [lang host] :or {lang :en host "localhost:3000"}}]
  ""
  {:pre [(not (nil? project))
         (not (nil? db))]}
  (->> (get-resource host :statement params)
       ; explanation
       (#(-> (select-keys % [:id :atom :main-issue :standard :weight :value])
             (filter-by :con %)
             (filter-by :pro %)
             (assoc :text (lang (:text %)))
             (assoc :header (lang (:description (:header %))))))
       (#(update-in % [:pro] (fn [id] arg-id->premises [project db id] :lang lang :host host)))))

(defn get-nodes [project db id & {:keys [lang host] :or {lang :en host "localhost:3000"}}]
  (let [[info outline refs]
        [(get-metadata [project db id] :host host :lang lang)
         (get-outline [project db] :host host :lang lang)
         (get-metadata [project db] :host host :lang lang)]]
    (-> {}
        (assoc :description (-> info :description lang))
        (assoc :issues (make-issues outline))
        (assoc :outline outline)
        (assoc :references (map (partial key-filter [:creator :date :identifier :title])
                                (filter filter-refs refs))))))

(defn get-issues
  [[project db :as params] & {:keys [lang host]}]
  (let [outline (get-outline [project db] :host host :lang lang)]
    (make-issues outline)))

(defn get-references
  [[project db :as params] & {:keys [lang host]}]
  (let [references (get-metadata [project db] :host host :lang lang)]
    (map (partial key-filter [:creator :date :identifier :title])
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

(defn get-theory
  [{:keys [pid tid scheme lang translate]}]
  (let [theory (assoc (project/load-theory pid tid) :id tid)
        ;; TODO: creates a simpler function that just returns the :translation key?
        translator (comp (tr/make-default-translator)
                          (ttr/make-language-translator (:language theory)))
        do-translation (or (= translate "t") (= translate "true"))]
    (cond (and do-translation scheme)
          (-> (t/find-scheme theory (symbol scheme))
              (ttr/translate-scheme translator lang))

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
