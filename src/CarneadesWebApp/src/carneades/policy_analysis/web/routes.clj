;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.policy-analysis.web.routes
  (:use compojure.core)
  (:require [compojure.route :as route]
            [carneades.policy-analysis.web.controllers.policy-simulation :as simulation]
            [carneades.policy-analysis.web.controllers.translation :as translation]))

(defroutes policy-analysis-routes
  (GET "/config" [] (simulation/dump-config))
  (GET "/viewsession" {session :session} (str session))
  (GET "/" [] (simulation/init-page))
  (POST "/questions" request (simulation/process-ajax-request request))
  ;; (POST "/Translation"
  ;;       {session :session  body :body params :params}
  ;;       (translation/process-ajax-request session body params))
  ;; (route/resources "/" {:root "carneades/public"})
  (route/resources "/" {:root "carneades/public"})
  ;; (route/not-found "Page not found")
  )
