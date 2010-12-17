;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.wizardsprotocol)

(defrecord StatementItem [stmt formatted] Object
  (toString
   [this]
   formatted))

(defrecord LiteralFormular [formid panel textfields])

(defprotocol SwingGoalWizard
  (set-main-issue [this mainissue])
  (set-abduction-busy [this busy])
  (get-proponent-panel [this])
  (get-abduction-panel [this])
  (get-positions-panel [this])
  (reset-position [this])
  (display-position [this position posidx nbpos statement-formatted])
  (display-statements [this statements statement-formatted])
  (set-first-position-button-listener [this f args])
  (set-last-position-button-listener [this f args])
  (set-previous-position-button-listener [this f args])
  (set-next-position-button-listener [this f args])
  (set-sort-by-listener [this f args])
  (get-sort-by-value [this])
  (set-minimize-button-listener [this f args])
  (get-minimize-value [this]))

(defprotocol SwingFindArgumentsWizard
  (set-goal [this text])
  (get-searchparameters-panel [this])
  (get-searcharguments-panel [this])
  (arguments-found [this found])
  (set-argumentsearch-busy [this busy]))

(defprotocol SwingInstantiateSchemeWizard
  (display-clause [this clause clauseidx nb-clauses statement-formatted])
  (conclusionmatches-button-selected? [this])
  (conclusionmatches-button-enabled? [this])
  (set-conclusionmatches-button-enabled [this enabled])
  (set-conclusionmatches-button-listener [this f args])
  (set-conclusion-statement [this stmt])
  (get-schemes-panel [this])
  (get-clauses-panel [this])
  (display-schemes [this descs])
  (set-filter-text-listener [this f args])
  (get-filter-text [this])
  (set-previous-clause-button-listener [this f args])
  (set-next-clause-button-listener [this f args])
  (create-literal-formular [this formid literal suggestions variables suffix listener args])
  (fillin-formular [this form var-values] "does nothing if var does not exist for the form")
  (display-suggestion [this formular suggestion n nb-suggestions])
  (display-no-suggestion [this formular])
  (trigger-formpanel-validator [this formular])
  )