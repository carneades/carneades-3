(ns carneades.web.outline
  (:use (carneades.engine statement argument)
        carneades.web.pack
        carneades.database.db))

(defn create-outline-helper
  [n depth]
  (cond (= depth 1)
        [n []]

        (statement? n)
        (let [procon (concat (map argument-data (:pro n)) (map argument-data (:con n)))]
          [n (vec (map #(create-outline-helper % (dec depth)) procon))])

        (argument? n)
        (let [premises (map (comp pack-statement :statement) (:premises n))]
          [n (vec (map #(create-outline-helper % (dec depth)) premises))])))

(defn create-outline
  [depth]
  [:root (vec (map #(create-outline-helper % depth) (map pack-statement (main-issues))))])
