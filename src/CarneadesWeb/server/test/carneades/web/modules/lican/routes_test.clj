(ns carneades.web.modules.lican.routes-test
  (:require [midje.sweet :refer :all]
            [ring.mock.request :refer :all]
            [taoensso.timbre :as timbre :refer [debug info spy]]
            [carneades.web.handler :refer [app]]
            [cheshire.core :refer [parse-string encode]]
            [carneades.engine.utils :as utils]))

(def base-url "/carneades/api")

(defn parse
  [s]
  (parse-string s true))

(defn post-request
  [url content]
  (app (-> (request :post
                    (str base-url url))
           (body (encode content))
           (content-type "application/json"))))

(fact "It is not possible to use a GPL library from an Apache software."
      (let [content {"legalprofile" ""
                     "licensetemplateuri" "http://www.markosproject.eu/ontologies/oss-licenses#GPL-2.0"
                     "usepropertyuris" ["http://www.markosproject.eu/ontologies/software#dynamicallyLinkedEntity"]
                     "swentityuris" ["http://markosproject.eu/kb/Library/549"]
                     }
            res (post-request "/lican/findsoftwareentitieswithcompatiblelicenses" content)
            body-content (parse (:body res))]
        body-content => '("http://markosproject.eu/kb/Library/549")))

(fact "The onlinetour is compatible with the GPL 3.0"
      (let [content {"legalprofile" ""
                     "softwareentity" "http://markosproject.eu/kb/SoftwareRelease/1970"
                     }
            res (post-request "/lican/findcompatiblelicenses" content)
            body-content (parse (:body res))]
        body-content => '("http://www.markosproject.eu/ontologies/oss-licenses#GPL-3.0")))
