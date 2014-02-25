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
