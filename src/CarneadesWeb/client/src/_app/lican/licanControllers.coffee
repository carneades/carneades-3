# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular',
  'angular-translate',
  '../common/services/notifications',
  '../common/services/markos',
  '../common/directives/questions/questions'
], (angular) ->
  markQuestionGroupAnswered = (questions) ->
    for question in questions.getCurrentQuestionGroup()
      question.answered = true

    undefined

  angular.module('lican.controllers', [
    'services.notifications',
    'services.markos',
    'directives.questions',
    'pascalprecht.translate'
  ])

  # Example of call http://localhost:8080/carneades/#/lican?entity=http:%2F%2Fmarkosproject.eu%2Fkb%2FSoftwareRelease%2F1970&legalprofile=1
  .controller('IntroCtrl', ($scope, $state, $stateParams, $translate, $q, entity, markos) ->
    $scope.viewLoading = true
    $q.all([entity]).then((data) ->
      $scope.viewLoading = false
    )

    sEntity = entity.get uri: $stateParams.entity, ->
      $scope.title = $translate.instant 'lican.title', {entity: sEntity.name}

    markos.setUserId $stateParams.userId

    if not $stateParams.debug
      $state.go 'lican.questions'

    $scope.startAnalysis = () ->
      $state.go 'lican.questions'
  )

  .controller('QuestionsCtrl', ($scope, $state, $stateParams, questions) ->
    $scope.viewLoading = true

    questions.analyse $stateParams.entity, $stateParams.legalprofile
    $scope.questionGroups = questions.getQuestionGroups()

    $scope.sendAnswer = (form) ->

      if form.$invalid
        console.log "the form is invalid"
      else
        $scope.viewLoading = true
        console.log 'sending answer'
        markQuestionGroupAnswered(questions)
        questions.sendAnswer()

    $scope.solution = questions.solution

    $scope.$watch 'questionGroups', ((questions) ->
      if questions.length > 0
        $scope.viewLoading = false
        ), true

    $scope.$watch 'solution', ((solution) ->
      if solution.db?
        $scope.viewLoading = false
        console.log 'solution found!'
        $state.transitionTo 'home.projects.project.outline', {pid: 'markos', db: solution.db}
      ), true

    undefined
  )
