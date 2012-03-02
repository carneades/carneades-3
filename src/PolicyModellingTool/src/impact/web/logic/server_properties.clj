(ns impact.web.logic.server-properties)

(defn- format-values
  [vals]
  (apply str (map #(str % "\n") vals)))

(defn server-properties
  []
  (str
   "========== JAVA PROPERTIES ==========\n\n\n"
   (format-values (System/getProperties))
   "\n\n\n========== ENVIRONMENT ==========\n\n\n"
   (format-values (System/getenv))))

