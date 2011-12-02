(ns carneades.web.application.createdb)

(def dbname "db8")

(use 'carneades.database.db)
(use 'carneades.engine.statement)
(use 'carneades.engine.argument)

(def db (make-database-connection dbname "root" "pw1"))
(create-argument-database dbname "root" "pw1")



(def not-property 
  (make-statement 
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
          :conclusion not-property 
          :premises [(pm possession-required), 
                     (pm no-possession),
                     (pm foxes-are-wild)]))

(create-argument db a1)
