# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-translate',
   './argumentControllers',
   '../../../common/resources/arguments',
   '../../../common/resources/projects',
   '../../../common/directives/evaluation-indicator/evaluation-indicator'
], (angular) ->
  angular.module('argument.states', [
    'argument.controllers',
    'resources.arguments',
    'directives.evaluationIndicator'
  ])

  .config ($stateProvider) ->
    states = [{
      name: "home.projects.project.argument"
      label: "Argument"
      url: "/:db/arguments/:aid"
      data:
        commands: ['home.projects.project.map','home.projects.project.outline']
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          templateUrl: "projects/project/argument/view.jade"
          controller: "ArgumentCtrl"
          resolve:
            argument: (ArgumentLoader, $stateParams) ->
              new ArgumentLoader($stateParams)
            project: (ProjectLoader, $stateParams) ->
              new ProjectLoader($stateParams)
      }
    ,
      {
      name: "home.projects.project.argument.edit"
      url: "/edit"
      data:
        commands: []
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          templateUrl: "projects/project/argument/edit.jade"
          controller: "ArgumentEditCtrl"
      }
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
