;; Copyright (c) 2010 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.examples.kwakman
  (:use carneades.engine.statement
        carneades.engine.argument
        carneades.engine.argument-graph
        carneades.engine.dublin-core
        carneades.database.import
        carneades.database.export 
        carneades.xml.caf.export
        carneades.maps.lacij
        carneades.engine.uuid
        carneades.engine.argument-evaluation
        carneades.engine.aspic)
  (:require [clojure.java.jdbc :as jdbc]
            [carneades.database.db :as db]
            [carneades.database.argument-graph :as ag-db]))

;; The Pierson vs. Post case.  Used to illustrate the use of
;; a scheme for "practical reasoning" in legal argument.
;; See Atkinson, Bench-Capon, McBurney, "Arguing About Cases
;; as Practical Reasoning",  ICAIL05.  The following is a recontruction
;; of the arguments in the published opinion, not a reconstruction
;; of the reconstruction in Katie's, Trevor's and Peter's paper. 

;; The full text of the decision can be found at:
;; http://www.saucyintruder.org/pages/pierson.html 

;; TO DO: extend to illustrate argumentation schemes, premises roles,
;; support for representing statements in multiple natural languages
;; and references to source documents.

(def kwakman1 
  (make-argument-graph 
    :header (make-metadata 
              :title "Reconsructions of Niko Kwakman's opinion Regarding Mandatory Minimum Sentences."
              :creator "Tom Gordon"
              :date "2012")
    
    :references 
    {"Kwakman2012a"
     (make-metadata
       :title "A Critical Analysis of Mandatory Minimum Sentences"
       :date "February 29, 2012"
       :identifier "http://www.rug.nl" 
       :creator "Niko Kwakman"
       :publisher "University of Groningen")}))

;;  Judge Tompkins Opinion, for the majority 

(def mandatory-sentences-are-good
  (make-statement 
    :main true
    :text {:en "Passing an act with mandatory minimum sentences for serious crimes is good."}))


(def kwakman2
  (-> kwakman1
      (question [mandatory-sentences-are-good])))


(defn -main []
  (let [dbname "kwakman"
        root "root"
        passwd "pw1"
        db (db/make-connection dbname root passwd)]
     (ag-db/create-argument-database "examples" dbname root passwd (make-metadata))
     (import-from-argument-graph db kwakman2 true)))


