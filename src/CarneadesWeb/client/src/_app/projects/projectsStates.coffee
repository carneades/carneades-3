# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global define
define [
  'angular',
  '../common/resources/projects'
], (angular) ->
  "use strict"
  angular.module('projects.states', [
    'resources.projects'
  ])

  .config ($stateProvider) ->
    states = [
      name: 'home.projects'
      label: 'state.home.projects.label'
      url: 'projects'
      views:
        "content@":
          templateUrl: 'projects/projects.jade'
          controller: ($scope, $location, $translate, projects, editorService) ->
            $scope = extend $scope,
              projects: projects
              ag: ag
              languages: editorService.getLanguages()
              onSave: _onSave
              onCancel: editorService.onCancel
              placeholderName: $translate.instant 'placeholder.name'
              placeholderTitle: $translate.instant 'placeholder.title'
              tooltipSave: $translate.instant 'tooltip.argumentgraph.save'
              tooltipCancel: $translate.instant 'tooltip.cancel'
              tooltipNewProject: $translate.instant 'tooltip.projects.new'
              projectProperties:
                name: ""
                title: ""
                properties:
                  description:
                    en: "a description"
              copylink: (pid) ->
                window.prompt("Copy to clipboard: Ctrl+C, Enter", $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/" + $scope.$state.href 'home.projects.project', pid: pid)
         

          resolve:
            projects: (MultiProjectLoader) ->
              return new MultiProjectLoader()
    ,
      name: 'home.projects.new'
      label: 'state.home.projects.new.label'
      url: '/new'
      views:
        "content@":
          templateUrl: 'projects/newProject.jade'
        
    ]

    angular.forEach states, (state) ->
      $stateProvider.state(state)
      undefined

    undefined
