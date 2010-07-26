(ns carneades.editor.core
  (:use carneades.editor.view.editorapplication
        carneades.editor.controller.listeners-register)
  (:import carneades.editor.view.editorapplication.SwingView)
  ;; (:gen-class)
  )

(defn start []
  (prn "Starting the Carneades Editor...")
  (let [view (SwingView.)]
    (init view)
    (register-listeners view)
    (show view)))

(defn -main [& args]
  (start))

(start)
