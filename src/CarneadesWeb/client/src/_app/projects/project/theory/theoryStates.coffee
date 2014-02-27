# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', './theoryControllers'], (angular) ->
  angular.module('theory.states', ['theory.controllers']).config( ['$stateProvider', ($stateProvider) ->
    states = [{
      name: "projects.project.theory"
      label: "Theory"
      url: "/theories/:tid?scrollTo"
      controller: 'TheoryCtrl'
      views: {
        "@":
          controller: 'TheoryCtrl'
          templateUrl: 'project/theory/theory.tpl.html'
          resolve:
            theory: ['$http', '$stateParams', ($http, $stateParams) ->
              $http(
               method: 'GET'
               url: "../api/projects/#{$stateParams.pid}/theories/#{$stateParams.tid}?translate=t"
              ).then (data) -> data.data
              ]
      }}]

    angular.forEach states, (state) -> $stateProvider.state state
  ])
