# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global define
define [
  'angular'
  'root'
  '../common/resources/projects'
], (angular, cn) ->
  "use strict"

  carneades = cn.carneades

  modules = [
    'resources.projects'
    ]

  module = angular.module 'projects.states', modules

  configure = ($stateProvider) ->
    states = [
        name: 'home.projects'
        label: 'state.home.projects.label'
        url: 'projects'
        views:
          "content@":
            templateUrl: 'projects/projects.jade'
            controller: 'ProjectController'
            resolve: ProjectController.$resolve
      ,
        name: 'home.projects.new'
        label: 'projects.new'
        url: '/new'
        views:
          "content@":
            templateUrl: 'projects/newProject.jade'
            controller: 'NewProjectCtrl'
      ]

    angular.forEach states, (state) -> $stateProvider.state(state)

  module.config configure


  class ProjectController extends carneades.Controller
    @.$inject = [
      "$scope"
      "$location"
      "projects"
      ]

    @.$resolve =
      projects: (MultiProjectLoader) ->
        return new MultiProjectLoader()


    constructor: (@scope, @location, @projects) ->
      @scope.copyLink = @.copyLink
      @scope.projects = @projects

    copyLink: (pid) ->
      window.prompt(
        "Copy to clipboard: Ctrl+C, Enter",
        [
          @location.protocol()
          "://"
          @location.host()
          ":"
          @location.port()
          "/carneades/"
          $scope.$state.href 'home.projects.project', pid: pid
        ].join ''
      )

  module.controller 'ProjectController', ProjectController

  class NewProjectCtrl extends carneades.Controller
    @.$inject = [
      "$scope"
      "$translate"
      "editorService"
      "$state"
      "Project"
      ]

    constructor: (@scope, @translate, @editorService, @state, @Project) ->
      console.log 'Project=', @Project
      
      @scope.languages = @editorService.getLanguages()
      @scope.onSave = @onSave
      @scope.onCancel = @editorService.onCancel
      @scope.placeholderName = @translate.instant 'placeholder.name'
      @scope.placeholderTitle = @translate.instant 'placeholder.title'
      @scope.placeholderTheory = @translate.instant 'placeholder.theory'
      @scope.tooltipSave = @translate.instant 'tooltip.argumentgraph.save'
      @scope.tooltipCancel = @translate.instant 'tooltip.cancel'
      @scope.tooltipNewProject = @translate.instant 'tooltip.projects.new'
      @scope.projectContent =
        name: ""
        properties:
          title: ""
          description:
            en: ""
          theory: "default/walton_schemes"
      
    onSave: () =>
      @Project.save({}, @scope.projectContent).$promise.then((s) =>
        url = 'home.projects.project'
        params =
          pid: @scope.projectContent.name
        @state.transitionTo url, params, reload: true
      )
    
  module.controller 'NewProjectCtrl', NewProjectCtrl
