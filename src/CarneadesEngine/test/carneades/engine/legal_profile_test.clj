(ns carneades.engine.legal-profile-test
  (:require [midje.sweet :refer :all]
            [carneades.engine.dublin-core :as dc]
            [carneades.engine.theory :as t]
            [carneades.engine.argument :as a]
            [carneades.engine.shell :as shell]
            [taoensso.timbre :as timbre :refer [debug info spy]]
            [carneades.engine.aspic :refer [aspic-grounded]]
            [carneades.maps.lacij :refer [export]]))

(def theory
  (t/make-theory
   :header
   (dc/make-metadata
    :title "A theory to test profiles."
    :description {:en "Here comes a nice description."})
   
   :language
   (t/make-language)

   :sections
   [(t/make-section
     :id 'section1
     :header (dc/make-metadata :title "Section 1"
                               :description {:en ""})

     :schemes
     [(t/make-scheme
       :id 's-building
       :weight 0.5
       :header (dc/make-metadata
                :title "Building rule.")
       :conclusion '(mayBuildHouse ?Person ?Terrain)
       :premises [(a/pm '(hasBuildingPermit ?Person ?Terrain))
                  (a/pm '(hasTerrain ?Person ?Terrain))])

      (t/make-scheme
       :id 's-building-permit
       :weight 0.5
       :header (dc/make-metadata
                :title "Permit rule.")
       :conclusion '(hasBuildingPermit ?Person ?Terrain)
       :premises [(a/pm '(authoritiesContacted ?Person ?Terrain))
                  (a/pm '(hasBuildingPlan ?Terrain))])

      (t/make-scheme
       :id 's-has-terrain
       :weight 0.5
       :header (dc/make-metadata
                :title "Terrain rule.")
       :conclusion '(hasTerrain ?Person ?Terrain)
       :premises [(a/pm '(isOwner ?Person ?Terrain))])
      ])]))

(let [query '(hasTerrain ?Person ?Terrain)
      facts '[(isOwner Tom t1-cotedazur)]
      engine (shell/make-engine 500 facts [(t/generate-arguments-from-theory theory)])
      ag (shell/argue engine aspic-grounded query)]
  (debug ag)
  (export ag "/tmp/ag1.svg"))
