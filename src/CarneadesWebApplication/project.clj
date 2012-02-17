(defproject carneades-web-application "1.0.0-SNAPSHOT"
  :description "Carneades Web Application"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [carneades-web-service "1.0.0-SNAPSHOT"]
                 [carneades-engine "2.0.0-SNAPSHOT"]
                 [hiccup "0.3.6"]
                 [enlive "1.0.0"]
                 [compojure "1.0.1" :exclusion [clojure]]
                 [ring/ring-servlet "1.0.1"]
                 ;; [lein-ring "0.4.5"] ;; for 'run-jetty' on self-executable JAR
                 ]
  :dev-dependencies [[lein-ring "0.5.4"]]
  :ring {:handler carneades.web.application.routes-war/app}
  ;; :main ^{:skip-aot true} ;; comment skip-aot to build a JAR
  ;; carneades.web.application.routes-selfexe
  )
