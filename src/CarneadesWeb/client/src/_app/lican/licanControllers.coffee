# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', 'angular-translate',
  '../common/services/notifications',
  '../common/directives/questions/questions'], (angular) ->
  
  markQuestionGroupAnswered = (questions) ->
    for question in questions.getCurrentQuestionGroup()
      question.answered = true

    undefined
  
  angular.module('lican.controllers', ['services.notifications', 'directives.questions',
    'pascalprecht.translate'])

  # Example of call http://localhost:8080/carneades/#/lican?entity=http:%2F%2Fmarkosproject.eu%2Fkb%2FSoftwareRelease%2F366
  .controller('IntroCtrl', ['$scope', '$state', '$stateParams', 'entity', '$translate',
  ($scope, $state, $stateParams, entity, $translate) ->

    # TODO: check success + error msg
    sEntity = entity.get uri: $stateParams.entity, ->
      $scope.title = $translate.instant 'lican.title', {entity: sEntity.name}

      $scope.startAnalysis = () ->
        $state.go('lican.questions')
    ])

  .controller('QuestionsCtrl', ['$scope', '$state', '$stateParams',
  'questions', 'notifications',
  ($scope, $state, $stateParams, questions, notifications) ->

    questions.analyse $stateParams.entity
      
    $scope.questionGroups = questions.getQuestionGroups();
    $scope.sendAnswer = () ->

      if $scope.questionsForm.$invalid
        # TODO: notification
        notifications.pushSticky 'The form is invalid'
        console.log "the form is invalid"
      else
        console.log('sending answer')
        markQuestionGroupAnswered(questions)
        questions.sendAnswer()

    $scope.solution = questions.solution
    $scope.$watch 'solution', ((solution) ->
      if solution.db?
        console.log 'solution found!'
        $state.transitionTo 'home.projects.project.outline', {pid: 'markos', db: solution.db}
      ), true

    undefined

    ])
