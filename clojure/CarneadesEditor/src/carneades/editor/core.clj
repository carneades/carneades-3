(ns carneades.editor.core
  (:use carneades.editor.view.editorapplication
        carneades.editor.controller.listeners-register)
  (:import carneades.editor.view.editorapplication.SwingView)
  ;; (:gen-class)
  )

(defn -main [& args]
  (prn "Starting the Carneades Editor...")
  (let [view (SwingView.)]
    (register-listeners view))
  (launch-application args))

(-main ())
