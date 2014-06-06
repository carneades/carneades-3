# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', 'angular-translate',
   './argumentControllers', '../../../common/resources/arguments',
   '../../../common/resources/projects',
   '../../../common/directives/evaluation-indicator/evaluation-indicator'],
(angular) ->
  angular.module('argument.states', ['argument.controllers', 'resources.arguments', 'directives.evaluationIndicator']).config ($stateProvider, $stateUtilProvider) ->
    helper = $stateUtilProvider.$get()
    states = [{
      name: "home.projects.project.argument"
      label: "Argument"
      url: "/:db/arguments/:aid"
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          templateUrl: "project/argument/view.tpl.html"
          controller: "ArgumentCtrl"
          resolve:
            argument: (ArgumentLoader, $stateParams) ->
              new ArgumentLoader($stateParams)
            project: (ProjectLoader, $stateParams) ->
              new ProjectLoader($stateParams)
        "subnav@":
          templateUrl: 'subnav.tpl.html'
          resolve: helper.builder().add('commands', helper.cmdBuilder('home.projects.project.map','home.projects.project.outline')).build()
          controller: 'SubnavController'
      }
    ,
      {
      name: "home.projects.project.argument.edit"
      url: "/edit"
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          templateUrl: "project/argument/edit.tpl.html"
          controller: "ArgumentEditCtrl"
        "subnav@":
          templateUrl: 'subnav.tpl.html'
          resolve: helper.builder().add('commands', helper.cmdBuilder()).build()
          controller: 'SubnavController'
      }
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
