# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular',
  './theoryControllers',
  '../../../common/resources/projects',
  '../../../common/resources/theory'
], (angular) ->
  angular.module('theory.states', [
    'theory.controllers',
    'resources.theories'
  ])

  .config ($stateProvider) ->
    states = [{
      name: "home.projects.project.theory"
      label: "state.home.projects.project.theory"
      url: "/theories/:tpid/:tid?scrollTo"
      data:
        commands: []
      views:
        "content@":
          controller: 'TheoryCtrl'
          templateUrl: 'projects/project/theory/theory.jade'
          resolve:
            theory: 'Theory'
            scroll: 'scroll'
            project: (ProjectLoader, $stateParams) ->
              new ProjectLoader($stateParams)

      }]

    angular.forEach states, (state) -> $stateProvider.state state
