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
      label: "Pid"
      views:
        "@":
          templateUrl: "project/outline/outline.tpl.html"
          controller: ($scope, project) ->
            $scope.project = project
          resolve:
            project: ($stateParams, ProjectLoader) ->
              new ProjectLoader($stateParams)
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
