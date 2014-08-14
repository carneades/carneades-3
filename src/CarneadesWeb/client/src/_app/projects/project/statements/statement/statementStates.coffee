# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular',
  'angular-capitalize-filter',
  '../../../../common/resources/statements',
  '../../../../common/resources/projects',
  './statementControllers'
], (angular) ->
  angular.module('statement.states', [
    'statement.controllers',
    'resources.statements',
    'resources.projects'
  ])

  .config ($stateProvider) ->
    states = [
      name: 'home.projects.project.statements.statement'
      label: 'Statement'
      url: '/:sid'
      data:
        commands: ['home.projects.project.map','home.projects.project.outline']
      views:
        'content@':
          templateUrl: 'projects/project/statements/statement/view.jade'
          controller: 'StatementViewCtrl'
          resolve:
            statement: (StatementLoader, $stateParams) ->
              return new StatementLoader $stateParams
    ,
      name: 'home.projects.project.statements.statement.edit'
      label: 'Edit statement'
      url: '/edit'
      data:
        commands: ['home.projects.project.map','home.projects.project.outline']
      views:
        'content@':
          templateUrl: 'projects/project/statements/statement/edit.jade'
          controller: 'StatementEditCtrl'
          resolve:
            project: (ProjectLoader, $stateParams) ->
              return new ProjectLoader $stateParams
            statement: (StatementLoader, $stateParams) ->
              return new StatementLoader $stateParams
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
