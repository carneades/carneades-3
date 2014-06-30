;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Legal profiles enable the parametrization of theories.
They are composed of a subset of rules from a theory. A value v is
associated to each rule in the subset. The arguments instantiated from
the rules in the subset have a premise added automatically, with the
corresponding value v. This premise acts as a kind of switch to
'activate' or 'deactivate' the rule."}
  carneades.engine.legal-profile)

(defn apply-legal-profile
  "Apply a legal profile to a theory. Return the transformed theory."
  [theory profile]
  )
