;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(defproject carneades-webapp/carneades-webapp "1.0.0-SNAPSHOT"
  :description "The Carneades Web Application"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/data.json "0.1.1"]
                 [compojure "1.0.4" :exclusion [clojure]]
                 [hiccup "0.3.6"]
                 [enlive "1.0.0"]
                 [ring/ring-servlet "1.0.1"]
                 [ring-middleware-format "0.2.3"]
                 [carneades-engine "2.0.0-SNAPSHOT"]
                 [carneades-web-service "1.0.0-SNAPSHOT"]
                 [org.clojars.pallix/mygengo "1.0.0"]
                 ;; [jayq "0.1.0-alpha4"]
                 [cc.qbits/jayq "0.1.0-alpha4-SNAPSHOT"]
                 [lein-ring "0.5.4"]
                 [org.clojure/tools.logging "0.2.3"]]
  :plugins [[lein-ring "0.7.1"]
            [lein-cljsbuild "0.2.8"]
            [lein-sub "0.2.3"]]
  ;; sub projects
  :sub ["../CarneadesEngine" "../CarneadesWebService" "../CarneadesExamples"]
  ; Enable the lein hooks for: clean, compile, test, and jar.
  :hooks [leiningen.cljsbuild]
  :cljsbuild {
    ; Configure the REPL support; see the README.md file for more details.
    :repl-listen-port 9000
    :repl-launch-commands
      ; Launch command for connecting the page of choice to the REPL.
      ; Only works if the page at URL automatically connects to the REPL,
      ; like http://localhost:3000/repl-demo does.
      ;     $ lein trampoline cljsbuild repl-launch firefox <URL>
      {"firefox" ["firefox"
                  :stdout ".repl-firefox-out"
                  :stderr ".repl-firefox-err"]
      ; Launch command for interacting with your ClojureScript at a REPL,
      ; without browsing to the app (a static HTML file is used).
      ;     $ lein trampoline cljsbuild repl-launch firefox-naked
       "firefox-naked" ["firefox"
                        "resources/private/html/naked.html"
                        :stdout ".repl-firefox-naked-out"
                        :stderr ".repl-firefox-naked-err"]
      ; This is similar to "firefox" except it uses PhantomJS.
      ;     $ lein trampoline cljsbuild repl-launch phantom <URL>
       "phantom" ["phantomjs"
                  "phantom/repl.js"
                  :stdout ".repl-phantom-out"
                  :stderr ".repl-phantom-err"]
      ; This is similar to "firefox-naked" except it uses PhantomJS.
      ;     $ lein trampoline cljsbuild repl-launch phantom-naked
       "phantom-naked" ["phantomjs"
                        "phantom/repl.js"
                        "resources/private/html/naked.html"
                        :stdout ".repl-phantom-naked-out"
                        :stderr ".repl-phantom-naked-err"]}
    :test-commands
      ; Test command for running the unit tests in "test-cljs" (see below).
      ;     $ lein cljsbuild test
      {"questions" ["casperjs"
                    "--dir=resources/carneades/private/html/"
                    "--url=test-questions.html"
                    "casper/run-unit-test.js"
               ]}
    ;; :crossovers [example.crossover]
    ;; :crossover-jar true
    :builds {
      ; This build has the lowest level of optimizations, so it is
      ; useful when debugging the app.
      :dev
      {:source-path "src-cljs"
       :jar true
       :compiler {:output-to "resources/carneades/public/js/compiled-app.js"
                  :optimizations :whitespace
                  :pretty-print true}}
      ; This build has the highest level of optimizations, so it is
      ; efficient when running the app in production.
      :prod
      {:source-path "src-cljs"
       :compiler {:output-to "resources/carneades/public/js/compiled-app.js"
                  :optimizations :advanced
                  :pretty-print false}}
      ; This build is for the ClojureScript unit tests that will
      ; be run via CasperJS.  See the casper/run-unit-test.js file
      ; for details on how it's run.
      :test
      {:source-path "test-cljs"
       :compiler {:output-to "resources/carneades/private/js/unit-test.js"
                  :optimizations :whitespace
                  :pretty-print true}}}}
  :ring {:handler carneades.web.routes-dev/carneades-webapp}
  :profiles {:standalone {:main carneades.web.routes-selfexe}
             :war {:ring {:handler carneades.web.routes-war/impact-app}
                   :sub ["../CarneadesWebService"]}}
  )

