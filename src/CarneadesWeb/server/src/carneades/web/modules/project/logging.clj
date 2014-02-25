(ns carneades.web.modules.project.logging
  (:require ;[carneades.web.modules.project.functions :refer [get-projects]]
            [dire.core :refer [with-pre-hook!]]
            [taoensso.timbre :as timbre :refer (trace debug info warn error fatal spy)]))

;; (with-pre-hook! #'get-resource  (fn
;;                                   ([] (debug "Retrieve all projects."))
;;                                   ([id] (debug "Retrieve projects:" id))))


