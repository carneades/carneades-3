define ['angular', '../../common/resources/projects'], (angular) ->
  angular.module('project.states', ['resources.projects'])
  .config ['$stateProvider', ($stateProvider) ->
    states = [
      name: "projects.project"
      url: "/:pid"
      views:
        "@":
          templateUrl: "project/outline/outline.tpl.html"
          controller: ['$scope', 'project', ($scope, project) ->
            $scope.project = project
            ]
          resolve:
            project: ["$stateParams", "ProjectLoader", ($stateParams, ProjectLoader) ->
              new ProjectLoader($stateParams)
            ]
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
  ]
