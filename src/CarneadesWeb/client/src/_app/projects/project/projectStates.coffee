car # Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-translate',
  './projectControllers',
  '../../common/resources/projects',
  '../../common/directives/metadata-editor/metadata-editor'
], (angular) ->
  angular.module('project.states', [
    'project.controllers',
    'resources.projects'
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
          controller: 'ProjectViewCtrl'
          resolve:
            Project: 'Project'
            project: ($stateParams, ProjectLoader) ->
              return new ProjectLoader $stateParams
    ,
      name: 'home.projects.project.new'
      url: '/create'
      label: "Create argument graph"
      views:
        "content@":
          templateUrl: 'projects/project/newArgGraph.jade'
          controller: 'ProjectNewArgGraphCtrl'
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined
    undefined
