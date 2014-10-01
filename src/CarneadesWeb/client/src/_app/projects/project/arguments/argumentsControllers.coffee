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
    object

  angular.module('arguments.controllers', [
    'pascalprecht.translate',
    'angular-capitalize-filter'
  ])

  .controller 'ArgumentNewCtrl', ($scope, $state, $stateParams,
  $translate, Argument, statements, breadcrumbService, theory, editorService) ->
    _normalize = () ->
      return extend {},
        header:
          title: ''
          description:
            en: ''
            de: ''
            fr: ''
            it: ''
            sp: ''
            nl: ''
        pro: true
        scheme: ''
        strict: false
        weight: 0.5
        conclusion: ''
        premises: []

    _addPremise = () ->
      editorService.addPremise $scope.argument

    _deletePremise = (p) ->
      editorService.deletePremise $scope.argument, p

    _showModel = () ->
      $scope.tabModel = true
      $scope.tabMetadata = false

    _showMetadata = () ->
      $scope.tabModel = false
      $scope.tabMetadata = true

    _getSchemeTitle = (model) ->
      return editorService.getSchemeTitle model, theory.schemes

    _getStatementText = (model) ->
      return editorService.getStatementText model, statements

    _onSave = () ->
      pid = $stateParams.pid
      db = $stateParams.db
      argument = Argument.save {
        pid: pid
        db: db
        header: $scope.argument.header
        pro: $scope.argument.pro
        scheme: "(#{$scope.argument.scheme})"
        weight: $scope.argument.weight
        conclusion: $scope.argument.conclusion
        premises: $scope.argument.premises
      }
      argument.$promise.then((a) ->
        url = 'home.projects.project.arguments.argument'
        params = pid: $stateParams.pid, db: $stateParams.db, aid: a.id
        $state.transitionTo url, params
      )

    $scope = extend $scope,
      statements: statements
      argument: _normalize()
      theory: theory
      addPremise: _addPremise
      deletePremise: _deletePremise
      tabModel: true
      tabMetadata: false
      showModel: _showModel
      showMetadata: _showMetadata
      onSave: _onSave
      onCancel: editorService.onCancel
      languages: editorService.getLanguages()
      getSchemeTitle: _getSchemeTitle
      getStatementText: _getStatementText
      editorOptions: editorService.getCodeMirrorOptions()
      title: $translate.instant 'projects.createargument'
      tooltipPremise: $translate.instant 'tooltip.premise'
      tooltipCancel: $translate.instant 'tooltip.cancel'
      tooltipSave: $translate.instant 'tooltip.argument.save'
