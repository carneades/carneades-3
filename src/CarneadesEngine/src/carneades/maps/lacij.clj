;;; Copyright © 2010-2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.maps.lacij
  (:use clojure.pprint
        carneades.engine.statement
        carneades.maps.lacij-export
        clojure.java.browse)
  (:import java.io.File))

;; We don't use a Protocol here since
;; they don't deal correctly with optional arguments
;; and overloading of functions definitions
;;

(defn export
  [ag filename & options]
  (export-ag ag literal->str filename (apply hash-map options)))

(defn export-str
  [ag lang & options]
  (export-ag-str ag #(literal->str % lang) (apply hash-map options)))

(defn view
  [ag & options]
  (let [tmpfile (File/createTempFile "carneadesmap" ".svg")
        filename (.getPath tmpfile)]
    ;; (.deleteOnExit tmpfile)
    (apply export ag filename options)
    (browse-url (str (.toURI tmpfile)))))

