# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', './theoryControllers'], (angular) ->
  angular.module('theory.states', ['theory.controllers']).config ($stateProvider) ->
    states = [{
      name: "home.projects.project.theory"
      label: "Theory"
      url: "/theories/:tid?scrollTo"
      controller: 'TheoryCtrl'
      commands: [
        label: "Outline"
        state: "home.projects.project.outline"
      ,
        label: "Theory"
        state: "home.projects.project.theory"
      ]
      views: {
        "@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          controller: 'TheoryCtrl'
          templateUrl: 'project/theory/theory.tpl.html'
          resolve:
            theory: ($http, $stateParams, $location) ->
              $http(
                method: 'GET'
                url: $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/#{$stateParams.pid}/theories/#{$stateParams.tid}?translate=t"
              ).then (data) -> data.data
             scroll: 'scroll' 
      }}]

    angular.forEach states, (state) -> $stateProvider.state state
