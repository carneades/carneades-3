# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular',
  'angular-resource',
  './licanControllers',
  './licanResources'
], (angular) ->
  angular.module('lican.states', [
    'ngResource',
    'lican.resources'
  ])

  .config ($stateProvider) ->
    states = [
      {
        name: 'lican'
        label: 'License Analyser Introduction'
        url: '/lican?entity&legalprofile&debug'
        views:
          "css@":
            template: '<css-inject theme="\'markos\'"></css-inject>'
          "banner@":
            template: '<project-banner theme="\'markos\'"></project-banner>'
          "footer@":
            template: '<project-footer theme="\'markos\'"></project-footer>'
          "nav@":
            template: "<bc-navigation ng-cloak></bc-navigation>"
          "content@":
            templateUrl: 'lican/introduction.jade'
            controller: 'IntroCtrl',
            resolve:
              entity: ($resource) ->
                $resource '../carneades/api/lican/entities/markos?uri=:uri'
      },
      {
        name: 'lican.questions'
        label: 'License Analyser Questions'
        url: '/questions'
        views:
          "nav@":
            template: "<bc-navigation></bc-navigation>"
          "content@":
            templateUrl: 'lican/questions.jade'
            controller: 'QuestionsCtrl'
            resolve:
              questions: (MultiQuestionLoader) ->
                return MultiQuestionLoader
      },
      {
        name: 'lican_demo'
        label: 'License Analyser Questions (Demo)'
        url: '/lican/demo'
        views:
          "nav@":
            template: "<bc-navigation></bc-navigation>"
          "content@":
            templateUrl: 'lican/questions.jade'
            controller: 'QuestionsCtrl'
            resolve:
              questions: (MultiQuestionLoader) ->
                return MultiQuestionLoader
      }
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
