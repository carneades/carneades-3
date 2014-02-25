(ns carneades.web.modules.project.logging
  (:require [carneades.web.modules.arguments.functions 
             :refer [get-argumentsgraph-info get-main-issues]]
            [dire.core :refer [with-pre-hook!]]
            [taoensso.timbre :as timbre :refer (trace debug info warn error fatal spy)]))

(with-pre-hook! #'get-argumentsgraph-info 
  (fn [project db id] 
    (debug "Retrieve arguments graph info: " project "/" db "/" id)))

(with-pre-hook! #'get-main-issues
  (fn [project db] 
    (debug "Retrieve main issues: " project "/" db)))
