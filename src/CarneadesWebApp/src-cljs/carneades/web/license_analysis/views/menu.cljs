(ns carneades.web.license-analysis.views.menu)

(defn lic-menu
  [project entity]
  [{:text :lic_menu_intro
    :link (str "#/license-analysis/introduction/" project "?entity=" entity)}
   {:text :pmt_menu_facts
    :link (str "#/policies/facts/" project)}
   {:text :lic_menu_theory
    :link (str "#/license-analysis/theory/" project)}
   {:text :pmt_menu_analysis
    :link (str "#/arguments/outline/" project "/" js/IMPACT.db)}])
