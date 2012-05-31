;;; Copyright (c) 2012 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.spocs-policies
  (:use carneades.engine.dublin-core
        carneades.engine.scheme
        carneades.engine.argument))

(def spocs-policies 
  (make-theory
   :header 
   (make-metadata :title "SPOCS"
                  :description {:en "TODO - Here some descriptions of the policies"})
   
   :language
   {}

   :sections
   []))
