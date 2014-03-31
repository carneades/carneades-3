# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', 'angular-resource', './licanControllers', './licanResources'], (angular) ->
  "use strict"
  angular.module('lican.states', ['ngResource', 'lican.resources'])
  .config ['$stateProvider',  ($stateProvider) ->
    states = [
      {
        name: 'lican'
        label: 'License Analyser Introduction'
        url: '/lican?entity'
        templateUrl: 'introduction.tpl.html'
        controller: 'IntroCtrl',
        resolve:
          entity: ['$resource', ($resource) ->
            $resource '../carneades/api/lican/entities/markos?uri=:uri']
        },
      {
        name: 'lican.questions'
        label: 'License Analyser Questions'
        url: '/questions'
        templateUrl: 'questions.tpl.html'
        controller: 'QuestionsCtrl'
        resolve:
          questions: ['MultiQuestionLoader', (MultiQuestionLoader) ->
            MultiQuestionLoader
            ]
      }
      {
        name: 'lican_demo'
        label: 'License Analyser Questions (Demo)'
        url: '/lican/demo'
        templateUrl: 'questions.tpl.html'
        controller: 'QuestionsCtrl'
        resolve:
          questions: ['DemoMultiQuestionLoader', (MultiQuestionLoader) ->
            MultiQuestionLoader
            ]
      }
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
  ]
