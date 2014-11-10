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
        label: 'state.lican.label'
        url: '/lican?entity&legalprofile&userId&debug'
        data:
          theme: 'markos'
        views:
          "header@":
            templateUrl: 'header.jade'
            controller: 'HeaderCtrl'
          "subnav@":
            template: '<page-navigation ng-show="commands.length > 0"><page-navigation-item cmd="c" ng-repeat="c in commands"></page-navigation-item></page-navigation>'
            controller: 'SubnavController'
          "content@":
            templateUrl: 'lican/introduction.jade'
            controller: 'IntroCtrl',
            resolve:
              entity: ($resource) ->
                $resource '../carneades/api/lican/entities/markos?uri=:uri'
      },
      {
        name: 'lican.questions'
        label: 'state.lican.questions.label'
        url: '/questions'
        views:
          "content@":
            templateUrl: 'lican/questions.jade'
            controller: 'QuestionsCtrl'
            resolve:
              questions: (MultiQuestionLoader) ->
                return MultiQuestionLoader
      },
      {
        name: 'lican_demo'
        label: 'state.lican.demo.label'
        url: '/lican/demo'
        views:
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
