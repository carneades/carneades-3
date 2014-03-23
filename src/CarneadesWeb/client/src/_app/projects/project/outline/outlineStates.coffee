# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', '../../../common/resources/projects', '../../../common/resources/nodes', '../../../common/resources/metadata'], (angular) ->
  "use strict"
  angular.module('outline.states', ['resources.projects', 'resources.nodes', 'resources.metadata', 'outline.controllers']).config(($stateProvider) ->
    states = [
      {
        name: 'home.projects.project.outline'
        label: 'Outline'
        url: '/:db/outline'
        commands: [
          label: "Map"
          state: "home.projects.project.map"
        ,
          label: "Theory"
          state: "home.projects.project.theory"
        ]
        views: {
          "@": {
            templateUrl: 'project/outline/outline.tpl.html',
            controller: 'OutlineCtrl',
            resolve: {
              project: ($stateParams, ProjectLoader) ->
                new ProjectLoader($stateParams)
              ,
              node: ($stateParams, NodeLoader) ->
                $stateParams.nid = 1;
                new NodeLoader($stateParams)
              ,
              metadata: ($stateParams, MetadataLoader) ->
                  $stateParams.mid = 1;
                  new MetadataLoader($stateParams)
            }
          }
        }
      }
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
  )
