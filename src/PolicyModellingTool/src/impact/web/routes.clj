(ns impact.web.routes
  (:use compojure.core)
  (:require [compojure.route :as route]
            [impact.web.controllers.policy-simulation :as simulation]
            [impact.web.controllers.policy-evaluation :as evaluation]
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
  (route/resources "/" {:root "policymodellingtool/public"})
  ;; (route/not-found "Page not found")
  )

