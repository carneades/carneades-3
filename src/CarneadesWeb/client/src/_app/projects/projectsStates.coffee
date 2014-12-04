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
          templateUrl: 'projects/list.jade'
          controller: 'ProjectController'
          resolve: ProjectController.$resolve
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
