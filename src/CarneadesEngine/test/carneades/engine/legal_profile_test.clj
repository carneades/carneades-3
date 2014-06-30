(ns carneades.engine.legal-profile-test
  (:require [midje.sweet :refer :all]
            [carneades.engine.dublin-core :as dc]
            [carneades.engine.theory :as t]
            [carneades.engine.argument :as a]
            [carneades.engine.shell :as shell]
            [taoensso.timbre :as timbre :refer [debug info spy]]
            [carneades.engine.aspic :refer [aspic-grounded]]
            [carneades.maps.lacij :refer [export]]
            [carneades.engine.legal-profile :refer [extend-theory]]
            [carneades.engine.argument-graph :as ag]
            [carneades.engine.argument-evaluation :refer [in-node?
                                                          out-node?]]))

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
       :premises [(a/pm '(hasTerrain ?Person ?Terrain))
                  (a/pm '(hasBuildingPermit ?Person ?Terrain))])

      (t/make-scheme
       :id 's-permit
       :weight 0.5
       :header (dc/make-metadata
                :title "Permit rule.")
       :conclusion '(hasBuildingPermit ?Person ?Terrain)
       :premises [(a/pm '(hasContactedAuthorities ?Person ?Terrain))
                  (a/pm '(hasBuildingPlan ?Terrain))])

      (t/make-scheme
       :id 's-terrain
       :weight 0.5
       :header (dc/make-metadata
                :title "Terrain rule.")
       :conclusion '(hasTerrain ?Person ?Terrain)
       :premises [(a/pm '(isOwner ?Person ?Terrain))
                  (a/pm '(Terrain ?Terrain))])
      ])]))

(facts "A profile without rules does not affect the evaluation of the
argument graph."
       (let [query '(mayBuildHouse ?Person ?Terrain)
             facts '[(Terrain t1-cotedazur)
                     (isOwner Tom t1-cotedazur)
                     (hasContactedAuthorities Tom t1-cotedazur)
                     (hasBuildingPlan t1-cotedazur)]
             profile {:metadata {:title "An empty profile"}
                      :default false
                      :rules []}
             theory' (extend-theory theory profile)
             engine (shell/make-engine
                     500
                     facts
                     [(t/generate-arguments-from-theory theory')])
             g (shell/argue engine aspic-grounded query)
             conclusion (ag/get-statement-node
                         g
                         '(mayBuildHouse Tom t1-cotedazur))]
         (expect (in-node? conclusion) => true)))

(fact "A profile with a rule value set to 0.0 make the corresponding built
argument unacceptable."
      (let [query '(mayBuildHouse ?Person ?Terrain)
            facts '[(Terrain t1-cotedazur)
                    (isOwner Tom t1-cotedazur)
                    (hasContactedAuthorities Tom t1-cotedazur)
                    (hasBuildingPlan t1-cotedazur)]
            profile {:metadata {:title "A profile with one rule"}
                     :default false
                     :rules [{:ruleid 's-permit
                              :value 0.0}]}
            theory' (extend-theory theory profile)
            engine (shell/make-engine
                    500
                    facts
                    [(t/generate-arguments-from-theory theory')])
            g (shell/argue engine aspic-grounded query profile)
            argid (first (:pro (ag/get-statement-node
                              g
                              '(hasBuildingPermit Tom t1-cotedazur))))
            switch-node (ag/get-statement-node
                         g
                         '(valid s-permit))
            arg (ag/get-argument-node g argid)]
        (expect (out-node? arg) => true)
        (expect (out-node? switch-node) => true)
        (export g "/tmp/ag1.svg")))

