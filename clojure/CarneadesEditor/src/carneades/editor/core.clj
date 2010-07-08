(ns carneades.editor.core
  (:use carneades.editor.view.editorapplication
        carneades.editor.controller.listeners-register)
  (:gen-class))

(defn -main [& args]
  (prn "Starting the Carneades Editor...")
  (register-listeners)
  (launch-application args))

;; printf broken in the JAR?
