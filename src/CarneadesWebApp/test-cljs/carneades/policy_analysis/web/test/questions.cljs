;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.policy_analysis.web.test.questions
  (:use [jayq.core :only [$]]
        [jayq.util :only [log clj->js]]
        [catb.views.pmt.questions :only [show-questions]]))

(def q-role-yn {:id 'q-role-yn
                   :category_name "License (Role Y/N)"
                   :hint "License information."
                   :text "Does X has a license to do Y?"
                   :role true
                   :yesnoquestion true})

(def q-role {:id 'q-role
             :category_name "Usage (Role)"
             :hint "Usage information."
             :text "Usage X for ?Y"
             :type '[commercial non-commercial]
             :typename ["commercial" "non-commercial"]
             :role true
             :min 1
             :max 1
             :yesnoquestion false})

(def q-concept {:id 'q-concept
                :category_name "Father (Concept Y/N)"
                :hint "Father information."
                :text "Is X the father of Y?"
                :concept true
                :min 1
                :max 1
                :yesnoquestion true})

(def q-predicate-yn {:id 'q-predicate-yn
                     :category_name "Type of use (Predicate Y/N)"
                     :hint "Type of use information."
                     :text "a uses b for commercial purposes"
                     :formalanswers ["yes" "no" "maybe"]
                     :answers ["Yes" "No" "Maybe"]
                     :min 1
                     :max 1
                     :yesnoquestion true})

(def q-predicate {:id 'q-predicate
                  :category_name "Type of use (Predicate)"
                  :hint "Type of use information."
                  :text "?a uses ?b for ?p purposes"
                  :widgets ["text" "text" "text"]
                  ;; :answers [[] [] '[standard professional]]
                  ;; :formalanswers [[] [] ["Standard" "Professional"]]
                  :min 1
                  :max 1
                  :yesnoquestion false})

(def questions [q-role-yn
                q-role
                q-concept
                q-predicate-yn
                q-predicate])

(defn ^:export prepare []
  (doseq [q questions]
    (show-questions
     (clj->js [q])
     ($ "#questions")
     (fn [_] (js/alert "on submit")))))

(defn ^:export run []
  true)