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

  .controller 'StatementNewCtrl', ($scope, $translate, $state,
  $stateParams, $q, $timeout, breadcrumbService, editorService, Statement) ->
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
      Statement.save({
        pid: pid
        db: db
        title: $scope.title
        standard: $scope.statement.standard
        text: $scope.statement.text
        header: $scope.statement.header
        pro: $scope.statement.pro
        main: $scope.statement.main
        weight: $scope.statement.weight
      }).$promise.then((s) ->
        url = 'home.projects.project.statements.statement'
        params = pid: pid, db: db, sid: s.id
        $state.transitionTo url, params
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
      onCancel: editorService.onCancel
      languages: editorService.getLanguages()
      tabModel: true
      tabMetadata: false
      showModel: _showModel
      showMetadata: _showMetadata
      editorOptions: editorService.getCodeMirrorOptions()
      tooltipSave: $translate.instant 'tooltip.statement.save'
      tooltipCancel: $translate.instant 'tooltip.cancel'
