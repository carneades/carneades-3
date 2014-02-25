define ["angular"], (angular) ->
  "use strict"
  angular.module("directives.questions", []).directive("questionGroup", ->
    restrict: "E"
    replace: true
    templateUrl: "directives/questions/questionGroup.tpl.html"
    scope:
      group: "=group"

  ).controller "RoleQuestionCtrl", ["$scope", ($scope) ->
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
  ]
