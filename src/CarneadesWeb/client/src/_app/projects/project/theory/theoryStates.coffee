# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', './theoryControllers', '../../../common/resources/projects'], (angular) ->
  angular.module('theory.states', ['theory.controllers']).config ($stateProvider, $stateUtilProvider) ->
    helper = $stateUtilProvider.$get()
    states = [{
      name: "home.projects.project.theory"
      label: "Theory"
      url: "/theories/:tpid/:tid?scrollTo"
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "subnav@":
          templateUrl: 'subnav.tpl.html'
          resolve: helper.builder().add('commands', helper.cmdBuilder()).build()
          controller: 'SubnavController'
        "content@":
          controller: 'TheoryCtrl'
          templateUrl: 'project/theory/theory.tpl.html'
          resolve:
            theory: ($http, $stateParams, $location) ->
              $http(
                method: 'GET'
                url: $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/#{$stateParams.pid}/theories/#{$stateParams.tpid}/#{$stateParams.tid}?translate=t"
              ).then (data) -> data.data

            scroll: 'scroll'

            project: (ProjectLoader, $stateParams) ->
              new ProjectLoader($stateParams)

      }]

    angular.forEach states, (state) -> $stateProvider.state state
