;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

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
            [carneades.database.db :as db]))

(defmacro with-db [db & body]   
  `(jdbc/with-connection 
           ~db
           (jdbc/transaction ~@body)))

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
        db (db/make-database-connection dbname root passwd)]
     (db/create-argument-database dbname root passwd (make-metadata))
     (import-from-argument-graph db kwakman2 true)))


