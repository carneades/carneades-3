# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular'
], (angular) ->
  extend = (object, properties) ->
    for key, val of properties
      object[key] = val
    return object

  return angular.module('project.controllers', [])

  .controller 'ProjectViewCtrl', ($scope, $stateParams, $state, $translate,
  $location, project) ->
    $scope.$state.$current.self.tooltip = project.title
    $stateParams.db = 'main'

    _newArgumentGraph = () ->
      $state.transitionTo 'home.projects.project.new', pid: project.id

    _copyLink = () ->
      window.prompt("Copy to clipboard: Ctrl+C, Enter", $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/" + $scope.$state.href 'home.projects.project', pid: project.id)

    $scope = extend $scope,
      project: project
      newArgumentGraph: _newArgumentGraph
      copyLink: _copyLink
      tooltipNew: $translate.instant 'tooltip.argumentgraph.new'

    return @

  .controller 'ProjectNewArgGraphCtrl', ($scope, $state,
  $stateParams, $translate, Project, breadcrumbService, editorService) ->
    ag =
      name: ""
      header:
        description:
          en: ""
          de: ""
          fr: ""
          it: ""
          sp: ""
          nl: ""
        title: ""

    _onSave = () ->
      _description = {}
      for k, v of $scope.ag.header.description
        _description[k] = editorService.htmlize v

      $scope.ag.header.description = _description
      project = {
        name: $scope.ag.name
        title: $scope.ag.title
        header: $scope.ag.header
      }

      Project.newArgumentGraph({pid: $stateParams.pid}, project).$promise.then((data) ->
        params = pid: $stateParams.pid, db: $scope.ag.name
        url = 'home.projects.project.outline'
        $state.transitionTo url, params, relaod: true
      )

    $scope = extend $scope,
      ag: ag
      languages: editorService.getLanguages()
      onSave: _onSave
      onCancel: editorService.onCancel
      placeholderName: $translate.instant 'placeholder.name'
      placeholderTitle: $translate.instant 'placeholder.title'
      tooltipSave: $translate.instant 'tooltip.argumentgraph.save'
      tooltipCancel: $translate.instant 'tooltip.cancel'

    return @

  .controller 'ProjectEditCtrl', ($scope, $state,
  $stateParams, $translate, metadata, Metadata,
  breadcrumbService, editorService) ->
    _onSave = () ->
      params = pid: $stateParams.pid, db: 'main', mid: 1
      # no put implemented yet
      Metadata.update(params, metadata).$promise.then((data) ->
        url = 'home.projects.project'
        $state.transitionTo url, $stateParams, reload: true
      )

    $scope = extend $scope,
      metadata: metadata
      languages: editorService.getLanguages()
      onSave: _onSave
      onCancel: editorService.onCancel
      tooltipSave: $translate.instant 'tooltip.ag.save'
      tooltipCancel: $translate.instant 'tooltip.cancel'

    return @
