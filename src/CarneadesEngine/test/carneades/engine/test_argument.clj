;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.test-argument
  (:use clojure.test
        carneades.engine.statement
        carneades.engine.argument))

(deftest test-argument-variables
  (let [p1 (pm '(age ?x ?y))
        p2 (pm '(father ?z ?y))
        cc '(mother ?k ?y)
        arg (argument 'arg1 'pro cc [p1 p2])
        vars (argument-variables arg)]
    (is (some #(= '?x %) vars))
    (is (some #(= '?y %) vars))
    (is (some #(= '?k %) vars))
    (is (some #(= '?z %) vars))))

(deftest test-premise-statement
  (let [stmt "Foxes are wild animals."]
    (is (= stmt (premise-statement (pm stmt))))
    (is (= (list 'not stmt)
           (premise-statement (ordinary-premise stmt false nil))))))

(deftest test-argument-variables
  (let [not-property
        (struct fatom
                "%s by pursing the ?animal did not acquire property in the %s"
                '(not-property ?person ?animal))
        possession-required
        "Property rights in wild animals may be acquired only by possession"
        no-possession (struct fatom "%s did not have possession of the %s"
                              '(no-possession ?person ?animal))
        are-wild (struct fatom "%s are wild" '(are-wild ?animals))
        arg (pro 'arg1 not-property (list (pm possession-required)
                                          (pm no-possession)
                                          (pm are-wild)))]
    (is (= '(?animal ?animals ?person) (sort (argument-variables arg))))))

(deftest test-add-premise
  (let [not-property
        "Post by pursing the fox did not acquire property in the fox"
        possession-required
        "Property rights in wild animals may be acquired only by possession"
        no-possession "Post did not have possession of the fox"
        foxes-are-wild "Foxes are wild animals"
        arg (pro 'arg1 not-property (list (pm possession-required)
                                          (pm no-possession))) 
        arg2 (add-premise arg (pm foxes-are-wild))]
    (is (= {:id 'arg1,
         :applicable false,
         :weight 0.5,
         :direction :pro,
         :conclusion
         "Post by pursing the fox did not acquire property in the fox",
         :premises
         '({:atom
            "Property rights in wild animals may be acquired only by possession",
            :polarity true,
            :role nil,
            :type :carneades.engine.argument/ordinary-premise}
           {:atom "Post did not have possession of the fox",
            :polarity true,
            :role nil,
            :type :carneades.engine.argument/ordinary-premise}),
         :scheme nil
         :title nil}
        arg))
    (is (= {:id 'arg1,
         :applicable false,
         :weight 0.5,
         :direction :pro,
         :conclusion
         "Post by pursing the fox did not acquire property in the fox",
         :premises
         '({:atom "Foxes are wild animals",
            :polarity true,
            :role nil,
            :type :carneades.engine.argument/ordinary-premise}
           {:atom
            "Property rights in wild animals may be acquired only by possession",
            :polarity true,
            :role nil,
            :type :carneades.engine.argument/ordinary-premise}
           {:atom "Post did not have possession of the fox",
            :polarity true,
            :role nil,
            :type :carneades.engine.argument/ordinary-premise}),
         :scheme nil
         :title nil}
        arg2))))

(deftest test-nodes
  (is (= {'father
       {'(father X Y)
        {:statement '(father X Y),
         :status :stated,
         :standard :pe,
         :acceptable false,
         :complement-acceptable false,
         :premise-of #{},
         :conclusion-of #{}}},
       'mother
       {{:form "blabla", :term '(mother X Y)}
        {:statement {:form "blabla", :term '(mother X Y)},
         :status :stated,
         :standard :pe,
         :acceptable false,
         :complement-acceptable false,
         :premise-of #{},
         :conclusion-of #{}},
        '(mother P A)
        {:statement '(mother P A),
         :status :stated,
         :standard :pe,
         :acceptable false,
         :complement-acceptable false,
         :premise-of #{},
         :conclusion-of #{}}}}
      (nodes (list '(mother P A) '(father X Y)
                   (struct fatom "blabla" '(mother X Y))) ))))

(deftest test-accept
  ;; test based on the Pierson-Post example
  (let [not-property (str "Post, by pursuing the fox,"
                          "did not acquire property in the fox.")
        possession-required (str "Property rights in wild animals may "
                                 "be acquired only by possession.")
        foxes-are-wild "Foxes are wild animals."
        no-possession (str "Post did not have possession of the fox.")
        pursuit-not-sufficient (str "Pursuit is not sufficient"
                                    "to acquire possession.")
        justinian "Justinian's Institutes"
        fleta "Fleta"
        bracton "Bracton"
        actual-possession-required "Actual corporal possession is required."
        puffendorf "Puffendorf"
        bynkershoek "Bynkershoek"
        mortally-wounded-deemed-possessed (str "Pursuit is sufficient to obtain"
                                               " possession when the animal "
                                               "is mortally wounded.")
        barbeyrac "Barbeyrac"
        grotius "Grotius"
        mortally-wounded "The fox was mortally wounded."
        land-owner-has-possession (str "The owner of land pursuing a "
                                       "livelihood with animals on his land is "
                                       "deemed to have possession of "
                                       "the animals.")
        livelihood-on-own-land "Post was pursing his livelihood on his own land"
        keeble "Keeble"
        certainty (str "A bright-line rule creates legal certainty, "
                       "preserving peace and order.")
        order "Peace and order is an important social value."
        a1 (make-arg a1 (pro not-property
                        (pm possession-required)
                        (pm no-possession)
                        (pm foxes-are-wild)))
        a2 (make-arg a2 (pro no-possession (pm pursuit-not-sufficient)))
        a3 (make-arg a3 (pro pursuit-not-sufficient (am justinian)))
        a4 (make-arg a4 (pro pursuit-not-sufficient (am fleta)))
        a5 (make-arg a5 (pro pursuit-not-sufficient (am bracton)))
        a6 (make-arg a6 (pro no-possession (pm actual-possession-required)))
        a7 (make-arg a7 (pro actual-possession-required (am puffendorf)))
        a8 (make-arg a8 (pro puffendorf (am bynkershoek)))
        a9 (make-arg a9 (con actual-possession-required 
                        (pm mortally-wounded-deemed-possessed)
                        (pm mortally-wounded)))
        a10 (make-arg a10 (pro mortally-wounded-deemed-possessed (am grotius)))
        a11 (make-arg a11 (pro mortally-wounded-deemed-possessed (am barbeyrac)))
        a12 (make-arg a12 (con actual-possession-required
                          (pm land-owner-has-possession)
                          (pm livelihood-on-own-land)))
        a13 (make-arg a13 (pro land-owner-has-possession (am keeble)))
        a14 (make-arg a14 (pro actual-possession-required 
                          (pm certainty)
                          (pm order)))
        args1 (argument-graph (list a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13
                                    a14))
        facts1 (list foxes-are-wild possession-required certainty order)
        tompkins (accept args1 facts1)]
    (is (= (into #{} (vals (:nodes tompkins)))
           #{{"Bynkershoek"
              {:statement "Bynkershoek",
               :status :stated,
               :standard :pe,
               :acceptable false,
               :complement-acceptable false,
               :premise-of #{'a8},
               :conclusion-of #{}}}
             {"Pursuit is sufficient to obtain possession when the animal is mortally wounded."
              {:statement
               "Pursuit is sufficient to obtain possession when the animal is mortally wounded.",
               :status :stated,
               :standard :pe,
               :acceptable true,
               :complement-acceptable false,
               :premise-of #{'a9},
               :conclusion-of #{'a11 'a10}}}
             {"Actual corporal possession is required."
              {:statement "Actual corporal possession is required.",
               :status :stated,
               :standard :pe,
               :acceptable true,
               :complement-acceptable false,
               :premise-of #{'a6},
               :conclusion-of #{'a7 'a12 'a14 'a9}}}
             {"Bracton"
              {:statement "Bracton",
               :status :stated,
               :standard :pe,
               :acceptable false,
               :complement-acceptable false,
               :premise-of #{'a5},
               :conclusion-of #{}}}
             {"A bright-line rule creates legal certainty, preserving peace and order."
              {:statement
               "A bright-line rule creates legal certainty, preserving peace and order.",
               :status :accepted,
               :standard :pe,
               :acceptable false,
               :complement-acceptable false,
               :premise-of #{'a14},
               :conclusion-of #{}}}
             {"Justinian's Institutes"
              {:statement "Justinian's Institutes",
               :status :stated,
               :standard :pe,
               :acceptable false,
               :complement-acceptable false,
               :premise-of #{'a3},
               :conclusion-of #{}}}
             {"Foxes are wild animals."
              {:statement "Foxes are wild animals.",
               :status :accepted,
               :standard :pe,
               :acceptable false,
               :complement-acceptable false,
               :premise-of #{'a1},
               :conclusion-of #{}}}
             {"Fleta"
              {:statement "Fleta",
               :status :stated,
               :standard :pe,
               :acceptable false,
               :complement-acceptable false,
               :premise-of #{'a4},
               :conclusion-of #{}}}
             {"Post, by pursuing the fox,did not acquire property in the fox."
              {:statement
               "Post, by pursuing the fox,did not acquire property in the fox.",
               :status :stated,
               :standard :pe,
               :acceptable true,
               :complement-acceptable false,
               :premise-of #{},
               :conclusion-of #{'a1}}}
             {"Grotius"
              {:statement "Grotius",
               :status :stated,
               :standard :pe,
               :acceptable false,
               :complement-acceptable false,
               :premise-of #{'a10},
               :conclusion-of #{}}}
             {"Post was pursing his livelihood on his own land"
              {:statement "Post was pursing his livelihood on his own land",
               :status :stated,
               :standard :pe,
               :acceptable false,
               :complement-acceptable false,
               :premise-of #{'a12},
               :conclusion-of #{}}}
             {"Keeble"
              {:statement "Keeble",
               :status :stated,
               :standard :pe,
               :acceptable false,
               :complement-acceptable false,
               :premise-of #{'a13},
               :conclusion-of #{}}}
             {"The owner of land pursuing a livelihood with animals on his land is deemed to have possession of the animals."
              {:statement
               "The owner of land pursuing a livelihood with animals on his land is deemed to have possession of the animals.",
               :status :stated,
               :standard :pe,
               :acceptable true,
               :complement-acceptable false,
               :premise-of #{'a12},
               :conclusion-of #{'a13}}}
             {"Barbeyrac"
              {:statement "Barbeyrac",
               :status :stated,
               :standard :pe,
               :acceptable false,
               :complement-acceptable false,
               :premise-of #{'a11},
               :conclusion-of #{}}}
             {"Post did not have possession of the fox."
              {:statement "Post did not have possession of the fox.",
               :status :stated,
               :standard :pe,
               :acceptable true,
               :complement-acceptable false,
               :premise-of #{'a1},
               :conclusion-of #{'a6 'a2}}}
             {"The fox was mortally wounded."
              {:statement "The fox was mortally wounded.",
               :status :stated,
               :standard :pe,
               :acceptable false,
               :complement-acceptable false,
               :premise-of #{'a9},
               :conclusion-of #{}}}
             {"Peace and order is an important social value."
              {:statement "Peace and order is an important social value.",
               :status :accepted,
               :standard :pe,
               :acceptable false,
               :complement-acceptable false,
               :premise-of #{'a14},
               :conclusion-of #{}}}
             {"Puffendorf"
              {:statement "Puffendorf",
               :status :stated,
               :standard :pe,
               :acceptable true,
               :complement-acceptable false,
               :premise-of #{'a7},
               :conclusion-of #{'a8}}}
             {"Property rights in wild animals may be acquired only by possession."
              {:statement
               "Property rights in wild animals may be acquired only by possession.",
               :status :accepted,
               :standard :pe,
               :acceptable false,
               :complement-acceptable false,
               :premise-of #{'a1},
               :conclusion-of #{}}}
             {"Pursuit is not sufficientto acquire possession."
              {:statement "Pursuit is not sufficientto acquire possession.",
               :status :stated,
               :standard :pe,
               :acceptable true,
               :complement-acceptable false,
               :premise-of #{'a2},
               :conclusion-of #{'a5 'a4 'a3}}}}))))