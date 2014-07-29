# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-translate',
   './agControllers',
   '../../../common/directives/metadata-editor/metadata-editor'
], (angular) ->
  angular.module('ag.states', [
    'ag.controllers',
    'directives.metadataEditor'
  ])

  .config ($stateProvider) ->
    states = [{
        name: "home.projects.project.create"
        url: '/create'
        label: "Create argument graph"
        data:
          commands: []
        views:
          "content@":
            templateUrl: 'projects/project/ag/create.jade'
            controller: 'CreateAgCtrl'
      }]

    angular.forEach states, (state) ->
      $stateProvider.state state
