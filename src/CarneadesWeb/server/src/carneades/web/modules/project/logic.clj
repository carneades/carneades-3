;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Basic functions for serving project requests"}
  carneades.web.modules.project.logic
  (:require [clojure.string :refer [join]]
            [clojure.set :as set]
            [clojure.zip :as z]
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
            [carneades.engine.utils :refer [dissoc-in serialize-atom unserialize-atom]]
            [carneades.engine.theory :as theory]
            [carneades.engine.theory.zip :as tz]
            [carneades.database.legal-profile :as lp]
            [carneades.web.modules.project.service :as s]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- set-description-text [lang m] (assoc m :description (-> m :description lang)))

(defn- filter-refs [x] (not (= (:key x) nil)))

(defn to-map
  [rec]
  (into {} rec))

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
  [project db & {:keys [lang] :or {lang :en}}]
  (make-node (get (:outline (s/get-outline project db)) 1) lang 0))

(defn get-sub-outline [outline id] outline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of service calls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn augment-project
  [lang project]
  (set-description-text lang project))

(defn get-project
  [& {:keys [id lang]}]
  (augment-project lang (s/get-project id)))

(defn get-projects
  [& {:keys [lang]}]
  (map (partial augment-project lang) (s/get-projects)))

(defn get-outline
  [[project db id :as params]
   & {:keys [lang] :or {lang :en k nil}}]
  (info "calling outline")
  (->> (make-outline project db :lang lang)
       (#(if-not (nil? id) (get-sub-outline % id) %))))

(defn get-metadata
  [[project db :as params] & {:keys [k lang]}]
  {:pre [(not (nil? project))
         (not (nil? db))]}
  (->> (s/get-metadata project db)
       (#(if (nil? k)
           %
           (filter (fn [x] (= (:key x) k)) %)))
       (to-map)))

(defn get-metadatum
  [[project db id :as params] & {:keys [k lang]}]
  {:pre [(not (nil? project))
         (not (nil? db))]}
  (->> (s/get-metadatum project db id)
       (#(if (nil? k)
           %
           (filter (fn [x] (= (:key x) k)) %)))
        (#(if (contains? % :description)
           (set-description-text lang %)
           %))
        (to-map)))

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
      (assoc :text (or (-> conclusion :text lang) (serialize-atom (:atom conclusion))))
      ))

(defn trim-metadata
  "Removes unused languages and information from metadata"
  [metadata lang]
  (assoc metadata :description (lang (:description metadata))))

(defn trim-argument
  "Removes unused information from an argument (from the perspective
  of a statement)."
  [argument]
  (select-keys argument [:scheme :premises :id :conclusion :exceptions]))

(defn set-scheme-description-text
  [lang s]
  (update-in s [:header] (partial set-description-text lang)))

(defn set-schemes-description-text
  [lang section]
  (assoc section
    :header (set-description-text lang (:header section))
    :schemes (into [] (map (partial set-scheme-description-text lang)
                           (:schemes section)))))

(defn set-theory-description-text
  [theory lang]
  (let [t (update-in theory [:header] (partial set-description-text lang))
        t (tz/map-theory t (partial set-schemes-description-text lang))]
    t))

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
            (let [s (ttr/translate-scheme s translator lang)
                  s (set-scheme-description-text lang s)
                  s (assoc s :formalized true)]
              s))

          scheme
          (when-let [s (t/find-scheme theory (symbol scheme))]
            (let [s (set-scheme-description-text lang s)
                  s (assoc s :formalized true)]
              s))

          do-translation
          (let [t (ttr/translate-theory theory translator lang)
                t (set-theory-description-text t lang)
                t (dissoc t :language)]
            t)

          :else
          (-> theory
              (set-theory-description-text lang)
              (dissoc :language)))))

(defn get-theories
  [params]
  (let [params (merge {:lang :en} params)
        params (update-in params [:lang] keyword)]
    (if (:tid params)
      (get-theory params)
      (map #(get-theory (assoc params :tid %))
           (:theories (s/get-theories (:tpid params)))))))

(defn trim-scheme
  [scheme]
  (select-keys scheme [:id :header :formalized]))

(defn get-scheme-from-arg
  [project arg lang]
  (let [pcontent (s/get-project project)
        schemestr (str (first (unserialize-atom (:scheme arg))))
        schemes-project (theory/get-schemes-project project (:schemes pcontent))
        schemes-name (theory/get-schemes-name (:schemes pcontent))
        scheme (get-theory {:tpid schemes-project :tid schemes-name :scheme schemestr :lang lang})]
    (if (nil? scheme)
      ;; no scheme found in the theory? fake one
      {:header {:title schemestr}
       :id schemestr
       :formalized false}
      scheme)))

(defn augment-argument
  [arg project db lang]
  (let [scheme (get-scheme-from-arg project arg lang)]
    (-> arg
        (update-in [:header] trim-metadata lang)
        (update-in [:conclusion] trim-conclusion lang)
        (update-in [:premises] trim-premises lang)
        (assoc :scheme (trim-scheme scheme))
        (to-map))))

(defn get-argument
  [[project db id :as params]
   & {:keys [lang] :or {lang :en}}]
  (debug "get-argument")
  {:pre [(not (nil? project))
         (not (nil? db))]}
  (augment-argument (s/get-argument project db id) project db lang))

(defn get-arguments
  [project db lang]
  (map #(augment-argument % project db lang) (s/get-arguments project db)))

(defn get-trimed-argument
  [project db lang aid]
  (let [arg (get-argument [project db aid] :lang lang)]
    (trim-argument arg)))

(defn augment-statement
  [stmt project db lang]
  (let [stmt (update-in stmt [:header] trim-metadata lang)
        stmt (assoc stmt :text (or (lang (:text stmt)) (serialize-atom (:atom stmt))))
        stmt (assoc stmt :pro (map (partial get-trimed-argument project db lang)
                                   (:pro stmt)))
        stmt (assoc stmt :con (map (partial get-trimed-argument project db lang)
                                   (:con stmt)))
        stmt (assoc stmt :premise-of
                    (map (partial get-trimed-argument project db lang) (:premise-of stmt)))]
    (to-map stmt)))

(defn get-statements
  [project db lang]
  (map #(augment-statement % project db lang)
       (spy (s/get-statements project db))))

(defn get-statement
  [[project db id :as params]
   & {:keys [lang] :or {lang :en }}]
  {:pre [(not (nil? project))
         (not (nil? db))]}
  (let [stmt (s/get-statement project db id)]
    (augment-statement stmt project db lang)))

(defn get-nodes
  [project db id & {:keys [lang] :or {lang :en}}]
  (let [[info outline refs]
        [(get-metadata [project db id] :lang lang)
         (get-outline [project db] :lang lang)
         (get-metadata [project db] :lang lang)]]
    (-> {}
        (assoc :description (-> info :description lang))
        (assoc :issues (make-issues outline))
        (assoc :outline outline)
        (assoc :references (map #(select-keys % [:creator :date :identifier :title])
                                (filter filter-refs refs))))))

(defn get-issues
  [[project db :as params] & {:keys [lang]}]
  (let [outline (get-outline [project db] :lang lang)]
    (make-issues outline)))

(defn get-references
  [[project db :as params] & {:keys [lang]}]
  (let [references (s/get-metadata project db)]
    (map #(select-keys % [:creator :date :identifier :title :key])
         (filter filter-refs references))))

(defn get-argument-map [project db & {:keys [lang] :or {lang :en}}]
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

;; (defn get-project-archive
;;   [& {:keys [project host]}]
;;   (get-raw-resource host :export [(str project ".zip")]))

;; (defn post-project-archive
;;   [& {:keys [file host]}]
;;   ;; curl -F "file=@default2.zip;filename=nameinpost" http://localhost:8080/api/projects/upload
;;   (post-resource host :import []
;;                  {:multipart
;;                   [{:name "Content/type" :content "application/zip"}
;;                    {:name "file"
;;                     :content (clojure.java.io/file (.getPath (:tempfile file)))}]}))

(defn get-theme
  [[project did :as params]]
  (s/get-theme project did))

(def legal-profiles-user "root")
(def legal-profiles-password "pw1")

(defn get-profiles
  [pid]
  (lp/with-db pid legal-profiles-user legal-profiles-password
   (lp/read-profiles+)))

(defn get-profile
  [pid id]
  (lp/with-db pid legal-profiles-user legal-profiles-password
   (lp/read-profile+ id)))

(defn post-profile
  [pid profile]
  (lp/with-db pid legal-profiles-user legal-profiles-password
   (lp/create-profile+ profile)))

(defn put-profile
  [pid id update]
  (lp/with-db pid legal-profiles-user legal-profiles-password
    (let [pack-rule (fn [r]
                      (-> r
                          ;; (update-in [:value] #(Double/parseDouble %))
                          (update-in [:ruleid] unserialize-atom)))
          pack-rules (fn [rs] (map pack-rule rs))
          update (update-in update [:rules] pack-rules)]
      (lp/update-profile+ id update))))

(defn delete-profile
  [pid id]
  (lp/with-db pid legal-profiles-user legal-profiles-password
    (lp/delete-profile id)))
