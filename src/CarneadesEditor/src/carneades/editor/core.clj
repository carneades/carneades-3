;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.core
  (:use (carneades.editor.view viewprotocol)
        carneades.editor.view.application.editorapplication
        carneades.editor.controller.listeners.register
        carneades.editor.view.dialogs.properties 
        )
  (:gen-class))

(defn- log [logging]
  (when logging
    (prn "LOGGING activated")
    (let [date (.format (java.text.SimpleDateFormat. "yyyyMMdd-HHmmss")
                        (java.util.Date.))
          stream (java.io.FileWriter. (str "carneades-log-" date ".txt"))]
      (alter-var-root #'*out* (fn [_] stream))
      (alter-var-root #'*err* (fn [_] stream)))
    ))

(defn start []
  (prn "Starting the Carneades Editor...")
  (log true)
  (let [view (create-swingview)]
    (init view)
    (register-listeners view)
    (show view)))

(defn -main [& args]
  (start))

;; (start)
