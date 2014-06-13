# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define ["angular"], (angular) ->
  "use strict"
  angular.module("directives.questions", [])

  .directive("questionGroup", ->
    restrict: "E"
    replace: true
    templateUrl: "common/directives/questions/questionGroup.jade"
    scope:
      group: "=group"

  ).controller "RoleQuestionCtrl", ($scope) ->
    capitalize = (s) ->
      s[0].toUpperCase() + s.slice(1)

    $scope.isFactDeletionAllowed = () ->
      $scope.question.nbFacts != $scope.question.min

    $scope.isFactAdditionAllowed = (fact) ->
      [first...,lastFact] = $scope.question.facts
      (fact == lastFact) and ($scope.question.nbFacts != $scope.question.max)

    $scope.addFact = ->
      $scope.question.lastIndex++
      $scope.question.facts.push {index: $scope.question.lastIndex, input: undefined}
      $scope.question.nbFacts++

    $scope.deleteFact = (fact) ->
      $scope.question.nbFacts--
      $scope.question.facts = $scope.question.facts.filter (f) -> f != fact

    result = $scope.question.text.split(/\?\d+/)
    $scope.question.prefixText = capitalize(result[0])
    $scope.question.suffixText = result[1]
    $scope.question.jsType = typeof ($scope.question.type)
    undefined
