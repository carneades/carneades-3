# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', '../common/services/notifications', '../common/directives/questions/questions'], (angular) ->
  "use strict"

  markQuestionGroupAnswered = (questions) ->
    for question in questions.getCurrentQuestionGroup()
      question.answered = true

    undefined
  
  angular.module('lican.controllers', ['services.notifications', 'directives.questions'])

  # Example of call http://localhost:3000/carneades/#/lican?entity=http:%2F%2Fmarkosproject.eu%2Fkb%2FSoftwareRelease%2F366
  .controller('IntroCtrl', ['$scope', '$state', '$stateParams', 'messages', 'entity',
  ($scope, $state, $stateParams, messages, entity) ->

    # TODO: check success + error msg
    sEntity = entity.get uri: $stateParams.entity, ->
      $scope.title = messages.intro_title + sEntity.name
      $scope.messages = messages

      $scope.startAnalysis = () ->
        console.log('start analysis')
        $state.go('lican.questions')
    ])

  .controller('QuestionsCtrl', ['$scope', '$state', '$stateParams', 'messages',
  'questions', 'notifications',
  ($scope, $state, $stateParams, messages, questions, notifications) ->

    questions.analyse $stateParams.entity
      
    $scope.messages = messages
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
        $state.transitionTo 'projects.project.outline', {pid: 'markos', db: solution.db}
      ), true

    undefined

    ])
