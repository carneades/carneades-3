# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular'
  './projectDetailCtrl'
  './projectEditCtrl'
  './newArgumentGraphCtrl'
  'angular-translate'
  '../../common/resources/projects'
], (angular, ProjectViewController, ProjectEditController, NewArgumentGraphController) ->

  modules = [
    'resources.projects'
    'pascalprecht.translate'
    ]

  module = angular.module 'project.states', modules

  module.controller 'ProjectNewArgGraphCtrl', NewArgumentGraphController

  module.controller 'ProjectEditCtrl', ProjectEditController

  module.controller 'ProjectViewCtrl', ProjectViewController

  configure = ($stateProvider) ->
    tplProjectNew = """
    <page-navigation-sm-offset-2 ng-show="commands.length > 0">
      <page-navigation-item cmd="c" ng-repeat="c in commands">
    </page-navigation-item></page-navigation-sm-offset-2>
    """

    tplProjectEdit = """
    <page-navigation-sm-offset-2 ng-show="commands.length > 0">
      <page-navigation-item cmd="c" ng-repeat="c in commands">
    </page-navigation-item></page-navigation-sm-offset-2>
    """

    states = [
      name: "home.projects.project"
      url: '/:pid'
      label: 'state.home.projects.project.label'
      views:
        "content@":
          templateUrl: 'projects/project/project.jade'
          controller: 'ProjectViewCtrl'
          controllerAs: 'detail'
          resolve: ProjectViewController.$resolve
    ,
      name: 'home.projects.project.new'
      url: '/new'
      label: "state.home.projects.project.new.label"
      views:
        "content@":
          templateUrl: 'projects/project/newArgGraph.jade'
          controller: 'ProjectNewArgGraphCtrl'
        "subnav@":
          template: tplProjectNew
          controller: 'SubnavController'
    ,
      name: 'home.projects.project.edit'
      url: '/edit'
      label: "state.home.projects.project.edit.label"
      views:
        "content@":
          templateUrl: 'projects/project/edit.jade'
          controller: 'ProjectEditCtrl'
          resolve: ProjectEditController.$resolve
        "subnav@":
          template: tplProjectEdit
          controller: 'SubnavController'
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state

  module.config configure
