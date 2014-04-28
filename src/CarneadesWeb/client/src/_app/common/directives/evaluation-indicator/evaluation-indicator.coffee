# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

# Display an image representing the value of the evaluation
define ['angular'], (angular) ->
  angular.module("directives.evaluationIndicator", [])
  .directive("evaluationIndicator", ->
    restrict: "E"
    scope:
      value: "=",
      size: "="
    link: (scope, element, attrs) ->
      getEvaluationClass = (value) ->
        if not value?
          "evaluation-undefined-#{scope.size}"
        else if value >= 0.75
          "evaluation-in-#{scope.size}"
        else if value <= 0.25
          "evaluation-out-#{scope.size}"
        else if value > 0.25 and value < 0.75
          "evaluation-undecided-#{scope.size}"

      element.children().addClass (getEvaluationClass scope.value)

  )
