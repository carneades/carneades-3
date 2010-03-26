(comment
Sample clojure source file
)
(ns carneades
    (:gen-class))

(defn -main
    ([greetee]
  (println (str "Hello " greetee "!")))
  ([] (-main "world")))
