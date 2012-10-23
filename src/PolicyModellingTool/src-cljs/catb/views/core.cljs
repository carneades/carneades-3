(ns catb.views.core
  (:use [jayq.util :only [log clj->js]]
        [catb.models.core :only [get-stmt get-arg]])
  (:require [catb.backbone.core :as bb]
            [catb.template :as tp]))

(defn template
  "Replace the inner HTML of the View with the content of the template
   fill in with the passed variables."
  [view template_key variables]
  (bb/html view (tp/get template_key variables)))

(defn json
  "Returns the JSON content of a model."
  [model]
  (.toJSON model))

(defn score-agreed?
  [score]
  (> score 0.99))

(defn agreed?
  "Returns true if the claim was agreed."
  [claim votes]
  (let [vote (votes (.-id claim))]
    (and (not (nil? vote))
         (score-agreed? vote))))

(defn disagreed?
  "Returns true if the claim was disagreed."
  [claim votes]
  (let [vote (votes (.-id claim))]
    (and (not (nil? vote))
         (< vote 0.01))))

(defn prepare-claim
  "Assigns the text and the description of the statement to it."
  [statement-votes stmt]
  (let [text (js/AGB.statement_raw_text stmt)
        desc (js/AGB.description_text (.-header stmt))
        agreement_text (cond (agreed? stmt statement-votes)
                             (js/jQuery.i18n.prop "sct_agree")
                             (disagreed? stmt statement-votes)
                             (js/jQuery.i18n.prop "sct_disagree")
                             :else "")]
    (aset stmt "statement_text" text)
    (aset stmt "statement_description" desc)
    (aset stmt "agreement_text" agreement_text)
    stmt))


(defn prepare-argument
  "Set the description text of the argument"
  [arg]
  (aset arg "description_text" (js/AGB.description_text (.-header arg))))

(defn pro-answered
  "Returns the pro answered arguments"
  [claim args argument-votes]
  (map (comp json (partial get-arg args))
       (filter (fn [id] (argument-votes id)) (.-pro claim))))

(defn con-answered
  "Returns the con answered args"
  [claim args argument-votes]
  (map (comp json (partial get-arg args))
       (filter (fn [id] (argument-votes id)) (.-con claim))))

(defn prepare-arguments
  "Prepares the arguments of a claim being edited"
  [claim args argument-votes]
  (let [;; keep only args that have been answered
        pro_answered (pro-answered claim args argument-votes)
        con_answered (con-answered claim args argument-votes)]
    (doseq [arg (concat pro_answered con_answered)]
      (prepare-argument arg))
    (aset claim "has_pro_answered" (pos? (count pro_answered)))
    (aset claim "has_con_answered" (pos? (count con_answered)))
    (aset claim "pro_answered" (clj->js pro_answered))
    (aset claim "con_answered" (clj->js con_answered))
    claim))
