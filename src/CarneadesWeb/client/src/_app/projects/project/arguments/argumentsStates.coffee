# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-translate',
  './argumentsControllers',
  '../../../common/resources/statements',
  '../../../common/resources/theory',
  '../../../common/directives/evaluation-indicator/evaluation-indicator',
  '../../../common/services/projectInfo'
], (angular) ->
  angular.module('arguments.states', [
    'arguments.controllers',
    'resources.statements',
    'resources.theories',
    'directives.evaluationIndicator',
    'services.projectInfo'
  ])

  .config ($stateProvider) ->
    states = [
      name: "home.projects.project.arguments"
      abstract: true
      url: "/:db/arguments"
      resolve:
        project: (ProjectLoader, $stateParams) ->
          return new ProjectLoader $stateParams
      views:
        "subnav@":
          template: '<page-navigation-sm-offset-2 ng-show="commands.length > 0"><page-navigation-item cmd="c" ng-repeat="c in commands"></page-navigation-item></page-navigation-sm-offset-2>'
          controller: 'SubnavController'
    ,
      name: "home.projects.project.arguments.new"
      label: 'state.home.projects.project.arguments.new.label'
      url: '/new'
      data:
        commands: [
          'home.projects.project.map',
          'home.projects.project.outline'
        ]
      views:
        "content@":
          templateUrl: "projects/project/arguments/argument/edit.jade"
          controller: 'ArgumentNewCtrl'
          resolve:
            project: (ProjectLoader, $stateParams) ->
              return new ProjectLoader $stateParams
            theory: (TheoryLoader, $stateParams, project, projectInfo) ->
              $stateParams.tpid = projectInfo.getTheoryProject project
              $stateParams.tid = projectInfo.getTheoryName project
              return new TheoryLoader $stateParams
            statements: (MultiStatementLoader, $stateParams) ->
              return new MultiStatementLoader $stateParams
            conclusion: () ->
              return null
    ,
      name: "home.projects.project.arguments.new.withConclusion"
      label: 'state.home.projects.project.arguments.new.label'
      url: '/:sid'
      data:
        commands: [
          'home.projects.project.map',
          'home.projects.project.outline'
        ]
      views:
        "content@":
          templateUrl: "projects/project/arguments/argument/edit.jade"
          controller: 'ArgumentNewCtrl'
          resolve:
            project: (ProjectLoader, $stateParams) ->
              return new ProjectLoader $stateParams
            theory: (TheoryLoader, $stateParams, project, projectInfo) ->
              $stateParams.tpid = projectInfo.getTheoryProject project
              $stateParams.tid = projectInfo.getTheoryName project
              return new TheoryLoader $stateParams
            statements: (MultiStatementLoader, $stateParams) ->
              params = pid: $stateParams.pid, db: $stateParams.db
              return new MultiStatementLoader params
            conclusion: (StatementLoader, $stateParams) ->
              return new StatementLoader $stateParams
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
