;; Copyright (c) 2010 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Import from XML to argument graphs using the Carneades Argument Format (CAF)."}
  carneades.xml.caf.import
  (:require [clojure.data.xml :as x]
            [clojure.java.io :as io]
            [taoensso.timbre :as timbre :refer [debug info spy]]
            [carneades.xml.validation :refer [create-validation-fn]]
            [clojure.data.zip.xml :refer [attr text xml->]]
            [clojure.zip :as z])
  (:refer-clojure :exclude [import]))   

(defn import-description
  [desc]
  (let [n (z/node desc)]
    {(keyword (-> n :attrs :lang))
     (first (:content n))}))

(defn import-metadata
  [md]
  (let [descs (map import-description (xml-> md :descriptions :description))
        attrs (:attrs (z/node md))]
    (debug descs)
    (if (empty? descs)
      attrs
      (merge attrs {:description (apply merge descs)}))))

(defn- import-statement
  [stmt]
  (debug "import-statement")
  (let [metadata (first (map import-metadata (xml-> stmt :metadata)))
        ]
    (debug "metadata" metadata)
    (debug (map z/node (xml-> stmt :descriptions)))))

(defn- import-caf
  [xml]
  (let [zipper (z/xml-zip xml)
        metadata (first (map z/node (xml-> zipper :metadata)))
        statements (doall (map import-statement (xml-> zipper :statements :statement)))
        ;; metadata (z/node (xml-> zipper :statements))
        ]
    )
  )

(defn import
  ([path-or-url]
     (let [xml (x/parse (io/reader path-or-url))]
       (import-caf xml)))
  ([path-or-url validate]
     (if validate
       (let [validator (create-validation-fn (.getPath (io/resource "test/schemas/CAF.xsd")))
             input (slurp path-or-url)
             result (validator input)]
         (if (:left result)
           (throw (ex-info "Invalid XML document." {:error (:left result)}))
           (import-caf (x/parse-str input))))
       (import path-or-url))))
