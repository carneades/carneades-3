define ['angular', './statementControllers', '../../../common/resources/statements'], (angular) ->
  angular.module('statement.states', ['statement.controllers', 'resources.statements'])
  .config ['$stateProvider', ($stateProvider) ->
    states = [
      name: 'projects.project.statement'
      label: 'Statement'
      url: '/:db/statements/:sid'
      views: {
        '@': {
          templateUrl: 'project/statement/view.tpl.html'
          controller: 'StatementCtrl'
          resolve: {
            statement: [
              "StatementLoader", "$stateParams", (StatementLoader, $stateParams) ->
                new StatementLoader($stateParams)
            ]
          }
        }
      }
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
    ]
