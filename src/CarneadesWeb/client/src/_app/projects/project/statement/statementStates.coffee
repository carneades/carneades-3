# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', './statementControllers', '../../../common/resources/statements'], (angular) ->
  angular.module('statement.states', ['statement.controllers', 'resources.statements'])
  .config ['$stateProvider', ($stateProvider) ->
    states = [
      name: 'home.projects.project.statement'
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
