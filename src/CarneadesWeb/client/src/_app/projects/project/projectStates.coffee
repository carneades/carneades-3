# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  '../../common/resources/projects'
], (angular) ->
  angular.module('project.states', [
    'resources.projects'
  ])

  .config ($stateProvider) ->
    states = [
      name: "home.projects.project"
      url: '/:pid'
      label: "Project"
      data:
        commands: ['home.projects.project.outline']
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          templateUrl: 'projects/project/project.jade'
          controller: ($scope, project) ->
            $scope.project = project
            $scope.$stateParams.mid = 1
            $scope.$stateParams.db = 'main'
            $scope.$stateParams.nid = 1
            $scope.$state.$current.self.tooltip = project.title
          resolve:
            project: ($stateParams, ProjectLoader) ->
              new ProjectLoader($stateParams)
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined
    undefined
