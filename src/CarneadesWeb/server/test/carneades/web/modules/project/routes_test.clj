(ns carneades.web.modules.project.routes-test
  (:require [midje.sweet :refer :all]
            [ring.mock.request :refer :all]
            [taoensso.timbre :as timbre :refer [debug info spy]]
            [carneades.web.handler :refer [app]]
            [cheshire.core :refer [parse-string encode]]))

;; TODO: creates a temporary project directory

(def base-url "/carneades/api")

(defn parse
  [s]
  (parse-string s true))

(fact "It is possible to post a profile and read it back."
      (let [profile {:metadata {:title "One profile"}
                     :default true}
            response (app (-> (request :post
                                       (str base-url
                                            "/projects/markos/legalprofiles/"))
                              (body (encode profile))
                              (content-type "application/json")))
            body (parse (:body response))
            id (:id body)
            response2 (app (-> (request :get
                                        (str base-url
                                             "/projects/markos/legalprofiles/"
                                             id))
                               (content-type "application/json")))
            profile' (parse (:body response2))]
        (expect (select-keys (:metadata profile') (keys (:metadata profile))) =>
                (:metadata profile))
        (expect (:default profile') => true)))
