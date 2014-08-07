# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-translate'
  '../../common/resources/projects',
  '../../common/directives/metadata-editor/metadata-editor',
  '../../common/resources/ag'
], (angular) ->
  angular.module('project.states', [
    'resources.projects',
    'resources.ag',
    'directives.metadataEditor',
    'pascalprecht.translate'
  ])

  .config ($stateProvider) ->
    states = [
      name: "home.projects.project"
      url: '/:pid'
      label: "Project"
      data:
        commands: ['home.projects.project.outline']
      views:
        "content@":
          templateUrl: 'projects/project/project.jade'
          controller: ($scope, $state, $stateParams, project) ->
            $scope.project = project
            $scope.$stateParams.mid = 1
            $scope.$stateParams.db = 'main'
            $scope.$stateParams.nid = 1
            $scope.$state.$current.self.tooltip = project.title
            $scope.open = () ->
              $state.transitionTo 'home.projects.project.create', {pid: $stateParams.pid}
          resolve:
            project: ($stateParams, ProjectLoader) ->
              return new ProjectLoader($stateParams)
    ,
      name: 'home.projects.project.create'
      url: '/create'
      label: "Create argument graph"
      views:
        "content@":
          templateUrl: 'projects/project/newArgGraph.jade'
          controller: ($scope, $state, $stateParams, ag) ->
            $scope.ag =
              name: "",
              header:
                description: {en: "", de: "", fr: "", it: "", sp: "", nl: ""},
                title: ""

            $scope.onSave = () ->
              ag.save($stateParams, $scope.ag).$promise.then(
                (v) ->
                  $state.transitionTo 'home.projects.project.outline', {pid: $stateParams.pid, db: $scope.ag.name}
                (e) ->
                  console.log 'error', e
              )
          resolve:
            ag: 'Ag'
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined
    undefined
