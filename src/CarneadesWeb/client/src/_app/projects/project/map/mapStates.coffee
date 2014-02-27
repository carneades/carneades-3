# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

  define ['angular'], (angular) ->
  angular.module('map.states', ["map.controllers"])
  .config(["$stateProvider", ($stateProvider) ->
    states = [
      name: "projects.project.map"
      label: "Map"
      url: "/:db/map"
      views:
        "@":
          templateUrl: "project/map/map.tpl.html"
          controller: "MapCtrl"
          resolve:
            map: ["$http", "$stateParams", ($http, $stateParams) ->
              $http(
                method: 'GET'
                url: "../api/projects/#{$stateParams.pid}/#{$stateParams.db}/map"
              ).then (data) -> data.data
            ]
    ]

    angular.forEach states, (state) -> $stateProvider.state state
  ])
