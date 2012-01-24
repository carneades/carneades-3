(defproject carneades-web-application "1.0.0-SNAPSHOT"
  :description "Carneades Web Application"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [carneades-web-service "1.0.0-SNAPSHOT"]
                 [carneades-engine "1.0.0-SNAPSHOT"]
                 [hiccup "0.3.6"]
                 [enlive "1.0.0"]
                 [compojure "1.0.1"]
;; [lein-ring "0.4.5"] ;; for 'run-jetty' on self-executable JAR
                 ]
  :dev-dependencies [[swank-clojure "1.4.0-SNAPSHOT"]
                     [org.clojars.rayne/autodoc "0.8.0-SNAPSHOT"]
                     [lein-ring "0.4.5"]]
  :ring {:handler carneades.web.application.routes/app}
  :main ^{:skip-aot true}
  carneades.web.application.routes)
