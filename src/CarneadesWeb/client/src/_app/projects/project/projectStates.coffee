# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', '../../common/resources/projects'], (angular) ->
  angular.module('project.states', ['resources.projects'])
  .config ($stateProvider) ->
    states = [
      name: "home.projects.project"
      url: "/:pid"
      label: "Project"
      views:
        "banner@":
          template: "<project-banner></project-banner>"
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          templateUrl: 'project/project.tpl.html'
          controller: ($scope, project) ->
            $scope.project = project
            $scope.$stateParams.mid = 1
            $scope.$stateParams.db = 'main'
            $scope.$stateParams.nid = 1
            $scope.$state.$current.self.tooltip = project.title
          resolve:
            project: ($stateParams, ProjectLoader) ->
              new ProjectLoader($stateParams)
      commands: [
        label: "Arguments"
        state: "home.projects.project.outline"
      ,
        label: "Guided Tour"
        state: "home.projects"
      ,
        label: "Policies"
        state: "home.projects"
      ]
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
