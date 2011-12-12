(ns impact.web.main
  (:use ring.adapter.jetty
        clojure.contrib.javadoc.browse)
  (:require impact.web.routes))

(defn -main [& args]
  (let [port 8080
        url (str "http://localhost:" port)]
    (run-jetty #'impact.web.routes/app {:join? false :port 8080})
    (open-url-in-browser url)))


