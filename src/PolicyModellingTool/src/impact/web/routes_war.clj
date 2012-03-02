(ns impact.web.routes-war
  (:use carneades.web.service
        compojure.core
        impact.web.views.pages
        impact.web.logic.server-properties
        ring.middleware.params
        ring.middleware.session
        [hiccup.middleware :only (wrap-base-url)])
  (:require [compojure.route :as route]
            [compojure.handler :as handler]
            [compojure.response :as response]
            [impact.web.controllers.policy-simulation :as simulation]
            [impact.web.controllers.policy-evaluation :as evaluation]
            [impact.web.controllers.svg :as svg]
            [impact.web.controllers.translation :as translation]))


(defroutes impact-pm-tool-routes
  
  (GET "/viewsession" {session :session} (str session))
  (GET "/resetsession" [] (simulation/reset-session))
  (GET "/" [] (simulation/init-page))
  (POST "/PolicySimulation"
        {session :session body :body params :params}
        (simulation/process-ajax-request session body params))
  (POST "/PolicyEvaluation"
        {session :session  body :body params :params}
        (evaluation/process-ajax-request session body params))
  (POST "/Translation"
        {session :session  body :body params :params}
        (translation/process-ajax-request session body params))
  (route/resources "/")
  ;; (route/not-found "Page not found")
  )

(def impact-app
  (-> (handler/site impact-pm-tool-routes)
      (wrap-base-url)))
