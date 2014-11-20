# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular'
], (angular) ->
  typeIsObject = ( value ) ->
    return {}.toString.call(value) is '[object Object]'

  extend = (object, properties) ->
    for key, val of properties
      object[key] = val
    object

  secureExistence = (object) ->
    for key, val of object
      if typeIsObject val
        object[key] = secureExistence val
      else
        object[key] = object[key] ? ''
    object

  return angular.module('statement.controllers', [
    'pascalprecht.translate'
  ])

  .controller 'ModalStatementRemoveCtrl', ($scope, $modalInstance) ->
    $scope.ok = () ->
      $modalInstance.close true

    $scope.cancel = () ->
      $modalInstance.dismiss 'cancel'

    return @

  .controller 'StatementViewCtrl', ($scope, $state, $stateParams,
  $translate, $modal, statement, Statement, project, editorService) ->
    _remove = () ->
      modalInstance = $modal.open(
        templateUrl: 'projects/project/statements/statement/modalStatementRemove.jade'
        controller: 'ModalStatementRemoveCtrl'
      )

      modalInstance.result.then((m) ->
        Statement.delete($stateParams, statement).$promise.then((data) ->
          url = 'home.projects.project.outline'
          $state.transitionTo url, $stateParams, reload: true
        )
      )

    _getValueText = ({value}) ->
      key = 'projects.statement.value.uncertain'
      if value <= 0.25
        key = 'projects.statement.value.false'
      else if value >= 0.75
        key = 'projects.statement.value.true'

      return $translate.instant key

    _getTooltip = ({text}) ->
      return  [text.substring(0, 100), '...'].join ''

    _isHeaderEmpty = ({header}) ->
      return (v for k,v of header when v?).length == 0

    _getArgumentName = ({scheme}, idx) ->
      return if scheme?
        scheme.header.title
      else ($translate.instant 'projects.argument') + " ##{idx+1}"

    _showModel = () ->
      $scope.tabModel = true
      $scope.tabMetadata = false

    _showMetadata = () ->
      $scope.tabModel = false
      $scope.tabMetadata = true

    _edit = () ->
      url = 'home.projects.project.statements.statement.edit'
      $state.transitionTo url, $stateParams

    _openArgumentEditor = () ->
      url = 'home.projects.project.arguments.new.withConclusion'
      $state.transitionTo url, $stateParams

    _openArgument = (id) ->
      url = 'home.projects.project.arguments.argument'
      params = pid: $stateParams.pid, db: $stateParams.db, aid: id
      $state.transitionTo url, params

    _openStatement = (id) ->
      url = 'home.projects.project.statements.statement'
      params = pid: $stateParams.pid, db: $stateParams.db, sid: id
      $state.transitionTo url, params

    statement.valueText = _getValueText statement
    $scope = extend $scope,
      statement: statement
      project: project
      headerIsEmpty: _isHeaderEmpty statement
      argumentName: _getArgumentName
      edit: _edit
      remove: _remove
      openArgumentEditor: _openArgumentEditor
      openArgument: _openArgument
      openStatement: _openStatement
      tabModel: true
      tabMetadata: false
      showModel: _showModel
      showMetadata: _showMetadata
      isMapInitialized: editorService.isMapInitialized
      tooltipEdit: $translate.instant 'tooltip.statement.edit'
      tooltipRemove: $translate.instant 'tooltip.statement.remove'
      tooltipNew: $translate.instant 'tooltip.argument.new'

    $state.$current.self.tooltip = _getTooltip statement

    return @

  .controller 'StatementEditCtrl', ($scope, $translate, $state, $stateParams,
    statement, Statement, project, breadcrumbService, editorService, $cnBucket) ->
    _showModel = () ->
      $scope.tabModel = true
      $scope.tabMetadata = false

    _showMetadata = () ->
      $scope.tabModel = false
      $scope.tabMetadata = true

    _onSave = () ->
      Statement.update($stateParams, statement).$promise.then((data) ->
        url = 'home.projects.project.statements.statement'
        $state.transitionTo url, $stateParams, reload: true
        $cnBucket.remove $state.$current)

    $scope = extend $scope,
      standards: editorService.fillWithPrefixSuffixes(
        [], 'projects.', ['pe', 'dv', 'cce', 'brd'])
      title: $translate.instant 'projects.editstatement'
      statement: statement
      languages: editorService.getLanguages()
      project: project
      tabModel: true
      tabMetadata: false
      showModel: _showModel
      showMetadata: _showMetadata
      onSave: _onSave
      onCancel: editorService.onCancel
      editorOptions: editorService.getCodeMirrorOptions()
      tooltipSave: $translate.instant 'tooltip.statement.save'
      tooltipCancel: $translate.instant 'tooltip.cancel'

    return @
