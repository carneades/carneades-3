# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
