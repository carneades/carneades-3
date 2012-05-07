(ns impact.web.routes
  (:use compojure.core)
  (:require [compojure.route :as route]
            [impact.web.controllers.policy-simulation :as simulation]
            [impact.web.controllers.translation :as translation]))

(defroutes impact-pm-tool-routes
  (GET "/config" [] (simulation/dump-config))
  (GET "/viewsession" {session :session} (str session))
  (GET "/" [] (simulation/init-page))
  (POST "/PolicySimulation"
        {session :session body :body params :params}
        (simulation/process-ajax-request session body params))
  (POST "/Translation"
        {session :session  body :body params :params}
        (translation/process-ajax-request session body params))
  (route/resources "/" {:root "policymodellingtool/public"})
  ;; (route/not-found "Page not found")
  )

