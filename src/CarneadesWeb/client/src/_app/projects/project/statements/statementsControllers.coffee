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

  angular.module('statements.controllers', [
    'pascalprecht.translate'
  ])

  .controller 'StatementNewCtrl', ($scope, $translate, $state, $cnBucket,
  $stateParams, $q, $timeout, $previousState, breadcrumbService, editorService,
  Statement) ->
    _statement =
      text:
        en: ""
        de: ""
        fr: ""
        it: ""
        sp: ""
        nl: ""
      header:
        title: ''
        description:
          en: ""
          de: ""
          fr: ""
          it: ""
          sp: ""
          nl: ""
      pro: true
      main: false
      weight: 0.5
      standard: 'pe'

    _onSave = () ->
      pid = $stateParams.pid
      db = $stateParams.db
      Statement.save({pid: pid, db: db}, extend $scope.statement,
        title: $scope.title
      ).$promise.then((s) ->
        url = 'home.projects.project.statements.statement'
        params = pid: pid, db: db, sid: s.id
        $state.go url, params
        $previousState.forget 'newStatementEditor'
        $cnBucket.remove $state.$current
      )

    _showModel = () ->
      $scope.tabModel = true
      $scope.tabMetadata = false

    _showMetadata = () ->
      $scope.tabModel = false
      $scope.tabMetadata = true

    $scope = extend $scope,
      title: $translate.instant 'projects.createstatement'
      statement: _statement
      standards: editorService.fillWithPrefixSuffixes(
        [], 'projects.', ['pe', 'dv', 'cce', 'brd'])
      onSave: _onSave
      onCancel: -> editorService.onCancel 'newStatementEditor'
      languages: editorService.getLanguages()
      tabModel: true
      tabMetadata: false
      showModel: _showModel
      showMetadata: _showMetadata
      editorOptions: editorService.getCodeMirrorOptions()
      tooltipSave: $translate.instant 'tooltip.statement.save'
      tooltipCancel: $translate.instant 'tooltip.cancel'
