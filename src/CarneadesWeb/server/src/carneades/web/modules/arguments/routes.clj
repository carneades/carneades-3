(ns carneades.web.modules.arguments.routes
  (:use compojure.core)
  (:require [clojure.string :as s]
            [carneades.web.modules.arguments.views.layout :as layout]
            [carneades.web.modules.arguments.functions
             :refer [get-argumentsgraph-info get-main-issues get-outline get-references]]
            [clj-json.core :as json]
            [markdown.core :as md]))

(defn json-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string data)})


(defn get-reference [project db k]
  (map #(select-keys % [:source :identifier]) (filter (fn [x] (= (:key x) k)) (get-references project db))))

(defn normalize [text]
  "Transforms a markdown string to html and removes all html tags."
  (s/replace (md/md-to-html-string text) #"(<([^>]+)> *)" ""))

(defn issues->html [issues lang]
  (reduce #(conj %1 {:id (:id %2)

                     :text (normalize (:en (:text %2)))

                     :url "a"}) #{} issues))

(defn arguments-graph-info-page [project db id lang]
  (let [[arg-info issues outline refs]
        [(get-argumentsgraph-info project db id)
         (get-main-issues project db)
         (get-outline project db)
         (get-references project db)]]
    (layout/render "home.html" (merge {:base "/carneades"}
                                      (merge arg-info {:description (md/md-to-html-string (-> arg-info :description :en))})
                                      {:issues (issues->html
                                                (map #(select-keys
                                                       %
                                                       [:id :text])
                                                     issues) :en)

                                       :outline outline

                                       :references (map #(select-keys
                                                          %
                                                          [:creator :date :identifier :title])
                                                        (filter (fn [x]
                                                                  (not (= (:key x) nil)))
                                                                refs))}))))

(defroutes arguments-routes*
  (GET "/metadata/:project/:db/:key" [project db key] (get-reference project db key))
  (GET "/outline/:project/:db" [project db] (arguments-graph-info-page project db 1 "en")))

(def arguments-routes (-> #'arguments-routes*))

(defroutes arguments-web-routes
  (ANY "*" [] (context "/arguments" [] arguments-routes)))


(defroutes arguments-api-routes*
  (GET "/metadata/:project/:db/:key" [project db key] (json-response (get-reference project db key))))

(defroutes arguments-api-routes (-> #'arguments-api-routes*))
