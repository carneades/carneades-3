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
