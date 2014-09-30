# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular',
  'angular-capitalize-filter',
  './statementsControllers',
  '../../../common/resources/statements'
], (angular) ->
  angular.module('statements.states', [
    'statements.controllers',
    'resources.statements',
    'angular-capitalize-filter'
  ])

  .config ($stateProvider) ->
    states = [
      name: 'home.projects.project.statements'
      abstract: true
      url: '/:db/statements'
      resolve:
        project: (ProjectLoader, $stateParams) ->
          return new ProjectLoader $stateParams
      views:
        "subnav@":
          template: '<page-navigation-sm-offset-2><page-navigation-item cmd="c" ng-repeat="c in commands"></page-navigation-item></page-navigation-sm-offset-2>'
          controller: 'SubnavController'
        "mobsubnav@":
          template: '<page-navigation-sm-offset-2><page-navigation-item cmd="c" ng-repeat="c in commands"></page-navigation-item></page-navigation-sm-offset-2>'
          controller: 'MobSubnavController'
    ,
      name: 'home.projects.project.statements.new'
      label: 'state.home.projects.project.statements.new.label'
      url: '/new'
      data:
        commands: ['home.projects.project.map','home.projects.project.outline']
      views:
        'content@':
          templateUrl: 'projects/project/statements/statement/edit.jade'
          controller: 'StatementNewCtrl'
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
