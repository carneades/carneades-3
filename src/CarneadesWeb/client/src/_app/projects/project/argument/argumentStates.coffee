define ['angular', './argumentControllers', '../../../common/resources/arguments'], (angular) ->
  angular.module('argument.states', ['argument.controllers', 'resources.arguments']).config ['$stateProvider', ($stateProvider) ->
    states = [{
      name: "projects.project.argument"
      label: "Argument"
      url: "/:db/arguments/:aid"
      views: {
        "@": {
          templateUrl: "project/argument/view.tpl.html"
          controller: "ArgumentCtrl"
          resolve: {
            argument: [
              "ArgumentLoader", "$stateParams", (ArgumentLoader, $stateParams) ->
                new ArgumentLoader($stateParams)
              ]
          }
        }
      }
      },
      {
        name: "projects.project.argument.edit"
        url: "/edit"
        views: {
          "@": {
            templateUrl: "project/argument/edit.tpl.html"
            controller: "ArgumentEditCtrl"
          }
        }
      }
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
  ]
