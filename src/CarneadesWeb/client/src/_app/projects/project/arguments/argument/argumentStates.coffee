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
  '../../../../common/services/projectInfo'
], (angular) ->
  angular.module('argument.states', [
    'argument.controllers',
    'resources.arguments',
    'directives.evaluationIndicator',
    'services.projectInfo'
  ])

  .config ($stateProvider) ->
    states = [
      name: "home.projects.project.arguments.argument"
      label: 'state.home.projects.project.arguments.argument.label'
      url: "/:aid"
      data:
        commands: ['home.projects.project.map','home.projects.project.outline']
      views:
        "content@":
          templateUrl: "projects/project/arguments/argument/view.jade"
          controller: "ArgumentViewCtrl"
          resolve:
            project: (ProjectLoader, $stateParams) ->
              return new ProjectLoader $stateParams
            argument: (ArgumentLoader, $stateParams) ->
              return new ArgumentLoader $stateParams
    ,
      name: "home.projects.project.arguments.argument.edit"
      label: "state.home.projects.project.arguments.argument.edit.label"
      url: "/edit"
      views:
        "content@":
          templateUrl: "projects/project/arguments/argument/edit.jade"
          controller: "ArgumentEditCtrl"
          resolve:
            project: (ProjectLoader, $stateParams) ->
              return new ProjectLoader $stateParams
            argument: (Argument, $stateParams) ->
              return Argument.getRaw {}, $stateParams
            statements: (MultiStatementLoader, $stateParams) ->
              return new MultiStatementLoader $stateParams
            theory: (TheoryLoader, $stateParams, project, projectInfo) ->
              $stateParams.tpid = projectInfo.getTheoryProject project
              $stateParams.tid = projectInfo.getTheoryName project
              return new TheoryLoader $stateParams
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
