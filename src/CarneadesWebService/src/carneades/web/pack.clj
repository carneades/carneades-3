(ns carneades.web.pack
  (:use carneades.engine.statement
        carneades.engine.utils
        carneades.engine.dublin-core
        carneades.engine.scheme
        carneades.engine.argument
        carneades.database.db) 
  (:require [clojure.string :as str]))

(defn zip-metadata-element
  "Zips a metadata element vector as a string"
  [element]
  (if (or (vector? element) (seq? element))
   (if (empty? element)
     nil
     (str/join ";" element))
   element))

(defn unzip-metadata-element
  "Unzips a metadata element string as a vector"
  [s]
  (if (string? s)
    (if (empty? s)
      nil
      (str/split s #";"))
    s))

(defn zip-metadata
  "Zips a map of metadata elements vector and converts it
   to a map of metadata elements strings"
  [md]
  (reduce (fn [md [k v]]
            (if (= k :description)
              md
              (assoc md k (zip-metadata-element v))))
          md
          md))

(defn unzip-metadata
  "Unzips a map of metadata elements string and converts it
   to a map of metadata elements vectors"
  [md]
  (reduce (fn [md [k v]]
            (if (= k :description)
              md
              (assoc md k (unzip-metadata-element v))))
          md
          md))

(defn pack-statement 
  [stmt]
  {:post [(not (vector? (:atom %)))
          (not (seq? (:atom %)))]}
  (cond (sliteral? stmt) (str stmt),
        (statement? stmt) (assoc stmt
                            :atom (when (:atom stmt)
                                    (str (literal-atom stmt)))
                            :header (unzip-metadata (:header stmt)))
        :else nil))

(defn unpack-statement
  "Converts a JSON string representing a statement to a statement object."
  [s]
  (cond (string? s) (safe-read-string s),  
        (map? s) (let [atomval (if (or (nil? (:atom s))
                                       (empty? (:atom s)))
                                 nil
                                 (safe-read-string (:atom s)))]
                   (assoc (map->statement (dissoc s :atom))
                     :standard (keyword (:standard s))
                     :atom atomval
                     :header (map->metadata (zip-metadata (:header s)))))
        :else nil))

(defn pack-argument
  [arg]
  (if (nil? arg)
    nil
    (merge arg
           {:scheme (str (:scheme arg)),
            :conclusion (pack-statement (:conclusion arg)),
            :premises (map (fn [p] (assoc p :statement 
                                          (pack-statement (:statement p))))
                           (:premises arg))
            :header (unzip-metadata (:header arg))})))

(defn unpack-argument [arg]
  (assoc arg
         :scheme (when (:scheme arg) (symbol (:scheme arg)))
         :conclusion (unpack-statement (:conclusion arg))
         :premises (map (fn [p]
                          (map->premise (assoc p :statement (unpack-statement (:statement p)))))
                        (:premises arg))
         :exceptions (map (fn [p]
                          (map->premise (assoc p :statement (unpack-statement (:statement p)))))
                          (:exceptions arg))
         :header (map->metadata (zip-metadata (:header arg)))))

(defn unpack-arg-attrs
  [attrs]
  (if (:header attrs)
    (assoc attrs :header (map->metadata (:header attrs)))
    attrs))

(defn unpack-subs 
  "Replace keywords by logical variables in the substitutions
   received from Web clients."
  [m]
  (zipmap (map (fn [key] (symbol (name key)))
               (keys m))
          (map safe-read-string (vals m))))

(defn argument-data
  "Returns the argument content"
  [id]
  {:pre [(or (symbol? id)
             (string? id))]}
  (pack-argument (read-argument (str id))))

(defn- argument-metadata
  "Returns the metadata of an argument in a map
   or an empty map of if the argument has no metadata"
  [id]
  {:pre [(symbol? id)]}
  (or (:header (argument-data id))
      {}))
