(defproject carneadesweb-mappingtool "0.1.0"
  :description "Mapping of a LKIF to SVG as webapp"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [compojure "0.6.4"]
                 [hiccup "0.3.6"]
                 [carneades-engine "1.0.0-SNAPSHOT"]
                 [carneades-mapcomponent "1.0.0-SNAPSHOT"]]
  :dev-dependencies [[lein-ring "0.4.5"]
                     [swank-clojure "1.4.0-SNAPSHOT"]
                     ]
  :ring {:handler carneades.web.routes/app})
