(ns carneades.web.modules.session.logic
  ^{:author "Sebastian Kaiser"
    :doc "Logic of session module"}
  (:require [sandbar.stateful-session :refer :all]
            [ring.middleware.session.cookie :refer :all]))

(defn session-put-language [lang]
  (if (nil? lang)
    (when (nil? (session-get :language)) (session-put! :language "en"))
    (when-not (= lang (session-get :language))
      (session-put! :language lang))))
