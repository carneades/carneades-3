# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global define
define ['angular', '../common/resources/projects'], (angular) ->
  "use strict"
  angular.module('projects.states', ['resources.projects']).config ['$stateProvider', ($stateProvider) ->
    states = [
      name: 'projects'
      label: 'Projects'
      url: '/projects'
      templateUrl: 'list.tpl.html'
      controller: ($scope, projects) ->
        $scope.projects = projects
        $scope.copyLink = (pid) ->
          window.prompt("Copy to clipboard: Ctrl+C, Enter", $scope.$state.href 'projects.project', pid: pid)
        undefined

      resolve:
        projects: ['MultiProjectLoader', (MultiProjectLoader) ->
          new MultiProjectLoader()
        ]
    ]

    angular.forEach states, (state) ->
      $stateProvider.state(state)
      undefined

    undefined
  ]
