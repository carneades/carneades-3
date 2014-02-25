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
            $resource '../api/lican/entities/markos?uri=:uri']
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
