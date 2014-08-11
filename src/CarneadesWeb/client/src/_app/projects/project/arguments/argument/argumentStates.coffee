# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-translate',
  './argumentControllers',
  '../../../../common/resources/arguments',
  '../../../../common/directives/evaluation-indicator/evaluation-indicator',
], (angular) ->
  angular.module('argument.states', [
    'argument.controllers',
    'resources.arguments',
    'directives.evaluationIndicator',
  ])

  .config ($stateProvider) ->
    states = [
      name: "home.projects.project.arguments.argument"
      label: "Argument"
      url: "/:aid"
      data:
        commands: ['home.projects.project.map','home.projects.project.outline']
      views:
        "content@":
          templateUrl: "projects/project/arguments/argument/view.jade"
          controller: "ArgumentViewCtrl"
          resolve:
            argument: (ArgumentLoader, $stateParams) ->
              new ArgumentLoader($stateParams)
    ,
      name: "home.projects.project.arguments.argument.edit"
      url: "/edit"
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          templateUrl: "projects/project/arguments/argument/edit.jade"
          controller: "ArgumentEditCtrl"
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
