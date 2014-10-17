# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-translate',
  './projectControllers',
  '../../common/resources/projects'
], (angular) ->
  angular.module('project.states', [
    'project.controllers',
    'resources.projects'
    'pascalprecht.translate'
  ])

  .config ($stateProvider) ->
    states = [
      name: "home.projects.project"
      url: '/:pid'
      label: 'state.home.projects.project.label'
      views:
        "content@":
          templateUrl: 'projects/project/project.jade'
          controller: 'ProjectViewCtrl'
          resolve:
            project: ($stateParams, ProjectLoader) ->
              return new ProjectLoader $stateParams
    ,
      name: 'home.projects.project.new'
      url: '/new'
      label: "state.home.projects.project.new.label"
      views:
        "content@":
          templateUrl: 'projects/project/newArgGraph.jade'
          controller: 'ProjectNewArgGraphCtrl'
        "subnav@":
          template: '<page-navigation-sm-offset-2 ng-show="commands.length > 0"><page-navigation-item cmd="c" ng-repeat="c in commands"></page-navigation-item></page-navigation-sm-offset-2>'
          controller: 'SubnavController'
    ,
      name: 'home.projects.project.edit'
      url: '/edit'
      label: "state.home.projects.project.edit.label"
      views:
        "content@":
          templateUrl: 'projects/project/edit.jade'
          controller: 'ProjectEditCtrl'
          resolve:
            metadata: ($stateParams, MetadataRawLoader) ->
              $stateParams.db = 'main'
              $stateParams.mid = 1
              return new MetadataRawLoader $stateParams
        "subnav@":
          template: '<page-navigation-sm-offset-2 ng-show="commands.length > 0"><page-navigation-item cmd="c" ng-repeat="c in commands"></page-navigation-item></page-navigation-sm-offset-2>'
          controller: 'SubnavController'
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined
    undefined
