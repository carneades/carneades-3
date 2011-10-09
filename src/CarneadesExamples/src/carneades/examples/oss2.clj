;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.oss2
  (:use clojure.pprint
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.lkif.import
        carneades.mapcomponent.export))

(defn main []
  (let [lkif (import-lkif "/home/pal/open_source_licensing_good/impact-full.xml" )
        ag (second (:ags lkif))]
    (export-ag ag statement-formatted "/tmp/impact.svg" 
               :layout :radial
               :radius 300
               :treeify true)))
