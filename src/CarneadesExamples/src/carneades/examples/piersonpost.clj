;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.piersonpost
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

(def pierson-post 
  (make-argument-graph 
    :header (make-metadata 
              :title "Pierson vs. Post Revisted ? A Reconstruction using the Carneades 
              Argumentation Framework"
              :creator "Thomas F. Gordon; Douglas Walton"
              :identifier "http://dl.acm.org/citation.cfm?id=1565233.1565257"
              :publisher "IOS Press"
              :format "pdf"
              :date "2006")
    
    :references 
    {"pierson-post"
     (make-metadata 
       :title "Pierson v. Post"
       :date "1805"
       :coverage "New York"
       :identifier "3 Cai. R. 175, 2 Am. Dec. 264" 
       :creator "Daniel Tompkins; Henry Brockholst Livingston"
       :publisher "Supreme Court of New York"
       :description {:en "Pierson v Post is an American legal case that is widely 
                          used in law schools for teaching property law."})}))

;;  Judge Tompkins Opinion, for the majority 

(def not-property 
  (make-statement 
    :main true
    :text {:en "Post, by pursuing the fox, did not acquire property in the fox."}))

(def possession-required 
  (make-statement 
    :text {:en "Property rights in wild animals may be acquired only by possession."}))

(def foxes-are-wild 
  (make-statement :text {:en "Foxes are wild animals."}))

(def no-possession
  (make-statement :text {:en "Post did not have possession of the fox."}))

(def pursuit-not-sufficient 
  (make-statement :text {:en "Pursuit is not sufficient to acquire possession."}))

(def justinian 
  (make-statement :text {:en "Justinian's Institutes"}))

(def fleta (make-statement :text {:en "Fleta"}))

(def bracton (make-statement :text {:en "Bracton"}))

(def actual-possession-required 
  (make-statement :text {:en "Actual corporal possession is required."}))

(def puffendorf (make-statement :text {:en "Puffendorf"}))

(def bynkershoek (make-statement :text {:en "Bynkershoek"}))

(def mortally-wounded-deemed-possessed  
  (make-statement :text {:en "Pursuit is sufficient to obtain possession when the animal
is mortally wounded."}))

(def barbeyrac (make-statement :text {:en "Barbeyrac"}))

(def grotius (make-statement :text {:en "Grotius"}))

(def mortally-wounded (make-statement :text {:en "The fox was mortally wounded."}))

(def land-owner-has-possession ; warrant
  (make-statement :text {:en "The owner of land pursuing a livelihood with animals 
  on his land is deemed to have possession of the animals."}))

(def livelihood-on-own-land 
  (make-statement :text {:en "Post was pursing his livelihood on his own land"}))

(def keeble (make-statement :text {:en "Keeble"})) ; backing

(def certainty     ; action
  (make-statement :text {:en "A bright-line rule creates legal certainty, preserving 
peace and order."}))

(def order         ; value
  (make-statement :text {:en "Peace and order is an important social value."}))

(def a1 (make-argument 
          :header (make-metadata :description {:en "The question ... is, whether ... 
          Post, by the pursuit with his hounds ... acquired ... property in, the fox ... 
          It is admitted that a fox is an animal ferae naturae, and that property in 
          such animals is acquired by occupancy only. These admissions narrow the 
          discussion to the simple question of what acts as occupancy .."})
          :conclusion not-property 
          :premises [(pm possession-required), 
                     (pm no-possession),
                     (pm foxes-are-wild)]))

(def a2 (make-argument 
          :conclusion no-possession 
          :premises [(pm pursuit-not-sufficient)]))

(def a3 (make-argument
          :header (make-metadata :description {:en "Justinian's Institutes, lib. 2, 
          tit. 1, s.13, ... adopt[s] the principle, that pursuit alone vests no property 
          or right in the huntsman; and that even pursuit, accompanied with wounding, 
          is equally ineffectual for that purpose, unless the animal be actually 
          taken."})
          :conclusion pursuit-not-sufficient 
          :premises [(pm justinian)]))

(def a4 (make-argument 
          :header (make-metadata :description {:en "... and Fleta, lib. 3, c.2, p. 175, 
          adopt[s] the principle, that pursuit alone vests no property or right in the 
          huntsman; and that even pursuit, accompanied with wounding, is equally 
          ineffectual for that purpose, unless the animal be actually taken."})
          :conclusion pursuit-not-sufficient
          :premises [(pm fleta)]))

(def a5 (make-argument
          :header (make-metadata :description {:en "The same principle is recognized by 
          Bracton, lib. 2, c.1, p. 8."})
          :conclusion pursuit-not-sufficient
          :premises [(pm bracton)]))

(def a6 (make-argument 
          :conclusion no-possession 
          :premises [(pm actual-possession-required)]))

(def a7 (make-argument 
          :header (make-metadata :description {:en "Puffendorf, lib. 4, c.6, s.2, and 
          10, defines occupancy of beasts ferae naturae, to be the actual corporal 
          possession of them ..."})
          :conclusion actual-possession-required
          :premises [(pm puffendorf)]))

(def a8 (make-argument 
          :header (make-metadata :description {:en "... and Bynkershoek is cited as 
          coinciding in this definition."})
          :conclusion puffendorf 
          :premises [(pm bynkershoek) ]))

(def a9 (make-argument 
          :header (make-metadata :description {:en "Barbeyrac, in his notes on 
          Puffendorf ... af?rms, that actual bodily seizure is not, in all cases, 
          necessary to constitute possession of wild animals. ... the mortal wounding 
          of such beasts, ... may ... be deemed possession ... "})
          :conclusion (neg actual-possession-required)
          :premises [(pm mortally-wounded-deemed-possessed), 
                     (pm mortally-wounded)]))

(def a10 (make-argument 
           :header (make-metadata :description {:en "Barbeyrac seems to 
           have adopted .... the more accurate opinion of Grotius .."})
           :conclusion mortally-wounded-deemed-possessed 
           :premises [(pm grotius)]))

(def a11 (make-argument 
           :conclusion mortally-wounded-deemed-possessed 
           :premises [(pm barbeyrac)]))

(def a12 (make-argument
           :header (make-metadata :description {:en "The case cited from 11 Mod. 74-130, 
           I think clearly distinguishable from the present; inasmuch as there the action 
           was for maliciously hindering and disturbing the plaintiff in the exercise and 
           enjoyment of a private franchise; and ... the ducks were in the plaintiff?s 
           decoy pond, and so in his possession ..."})
           :conclusion (neg actual-possession-required)
           :premises [(pm land-owner-has-possession), 
                      (pm livelihood-on-own-land)]))

(def a13 (make-argument 
           :conclusion land-owner-has-possession 
           :premises [(pm keeble)]))

; teleological argument 
(def a14 (make-argument 
           :header (make-metadata :description {:en "We are the more readily inclined to 
           confere possession or occupancy of beasts ferae naturae, within the limits 
           prescribed by the learned authors above cited, for the sake of certainty, and 
           preserving peace and order in society. If the ?rst seeing, starting, or 
           pursuing such animals, without having so wounded, circumvented or ensnared 
           them, so as to deprive them of their natural liberty, and subject them to the 
           control of their pursuer, should afford the basis of actions against others 
           for intercepting and killing them, it would prove a fertile source of 
           quarrels and litigation."})
           :conclusion actual-possession-required 
           :premises [(pm certainty),    ; policy/action
                      (pm order)]))      ; value promoted

(def tompkins 
  (-> pierson-post
      (enter-arguments [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14])
      (accept [foxes-are-wild possession-required certainty order])))

;(view tompkins)

;; Judge Livingston's dissent.

(def chased-by-big-dogs 
  (make-statement :text {:en "The fox was being chased by large hounds."}))

(def deemed-mortally-wounded 
  (make-statement :text {:en "A noxious animal being chased by large hounds shall be 
deemed mortally wounded."}))

(def protecting-farmers   ; value
  (make-statement :text {:en "Protecting farmers is an important social value."}))

(def encourage-hunting    ; policy/action
  (make-statement :text {:en "Encouraging hunting helps protect farmers 
  from noxious animals."}))

(def foxes-are-noxious
  (make-statement :text {:en "Foxes are noxious animals."}))

(def admitted-in-the-pleadings
  (make-statement :text {:en "It is admitted in the pleadings that a fox is a wild
and noxious beast."}))

(def a15 (make-argument 
           :conclusion mortally-wounded
           :premises [(pm deemed-mortally-wounded), 
                      (pm chased-by-big-dogs),
                      (pm foxes-are-noxious)]))

(def a16 (make-argument 
           :header (make-metadata :description {:en "By the pleadings it is admitted that 
           a fox is a ?wild and noxious beast?. His depredations on farmers and on barn 
           yards have not been forgotten; and to put him to death wherever found, 
           is allowed to be meritorious, and of public benefit. Hence it follows, that our 
           decision should have in view the greatest possible encouragement to the 
           destruction of an animal ... But who would keep a pack of hounds; or what 
           gentlemen, at the sound of the horn, and at peep of day, would mount his steed, 
           and for hours together, ?sub jove frigido? or a vertical sun, pursue the 
           windings of this wily quadruped, if, just as night came on, and his stratagems 
           and strength were nearly exhausted, a saucy intruder, who had not shared in 
           the honours or labours of the chase, were permitted to come in at the death, 
           and bear away in triumph the object of pursuit? ... After mature deliberation, 
           I embrace that of Barbeyrac ... If at liberty, we might imitate the courtesy 
           of a certain emperor, who ... ordained, that if a beast be followed with large 
           dogs and hounds, he shall belong to the hunter, not to the chance occupant;
           and in like manner, if he be killed or wounded with a lance or sword; but if 
           chased with beagles only, then he passed to the captor, not to the first 
           pursuer. ... a pursuit like the present ... must inevitably ... 
           terminate in corporal possession .."})
           :conclusion deemed-mortally-wounded 
           :premises [(pm protecting-farmers), 
                      (pm encourage-hunting)]))

(def a17 (make-argument
           :header (make-metadata :description {:en "... By the pleadings it is admitted 
           that a fox is a ?wild and noxious beast?. His depredations on farmers and on 
           barn yards have not been forgotten ..."})
           :conclusion foxes-are-noxious
           :premises [(pm admitted-in-the-pleadings)]))

(def livingston 
  (-> (make-argument-graph) 
      (enter-arguments [a15 a16 a17]) 
      (accept [chased-by-big-dogs])))

(def both (-> tompkins 
              (enter-arguments [a15 a16 a17])
              (accept [chased-by-big-dogs])))

(defn -main []
  (let [dbname "pierson-post"
        root "root"
        passwd "pw1"
        db (db/make-database-connection dbname root passwd)]
     (db/create-argument-database dbname root passwd (make-metadata))
     (import-from-argument-graph db both true)
     (argument-graph->xml (export-to-argument-graph db))))

