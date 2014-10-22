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

  onArgumentRetrieve = (scope) ->
    scope.schemeId = if scope.argument.scheme? and scope.argument.scheme != "" and scope.argument.scheme[0] == '('
      scope.argument.scheme.slice 1, -1
    else
      undefined

  onSchemeChange = (scope, newVal) ->
    if newVal == undefined
      return

    scope.argument.scheme = "(#{newVal})"


    if scope.currentScheme?
      premises = ({role: p.role, positive: true, implicit: false} for p in scope.currentScheme.premises)
      # set up the role of the premises but try keeping the statements
      i = 0
      for p in premises
        premise = if scope.argument.premises? and scope.argument.premises[i]?
          scope.argument.premises[i]

        if premise?
          p.statement = premise.statement
        i++

      scope.argument.premises = premises


  angular.module('argument.controllers', [
    'pascalprecht.translate',
    'angular-capitalize-filter'
  ])

  .controller 'ModalArgumentRemoveCtrl', ($scope, $modalInstance) ->
    $scope.ok = () ->
      $modalInstance.close true

    $scope.cancel = () ->
      $modalInstance.dismiss 'cancel'

    return @

  .controller 'ArgumentViewCtrl', ($scope, $state, $stateParams, $translate,
    $modal, argument, Argument, project, editorService) ->

    _remove = () ->
      modalInstance = $modal.open(
        templateUrl: 'projects/project/arguments/argument/modalArgumentRemove.jade'
        controller: 'ModalArgumentRemoveCtrl'
      )

      modalInstance.result.then((m) ->
        Argument.delete($stateParams, argument).$promise.then((data) ->
          url = 'home.projects.project.outline'
          $state.transitionTo url, $stateParams, reload: true
        )
      )

    _showModel = () ->
      $scope.tabModel = true
      $scope.tabMetadata = false

    _showMetadata = () ->
      $scope.tabModel = false
      $scope.tabMetadata = true

    _getValueText = ({value}) ->
      key = 'projects.argument.value.unclear'
      if value <= 0.25
        key = 'projects.argument.value.unacceptable'
      else if value >= 0.75
        key = 'projects.argument.value.acceptable'

      return $translate.instant key

    _getConclusionText = ({strict, pro}) ->
      _getStrict = (value) ->
        strict_pro = 'projects.strict_pro_conclusion'
        strict_con = 'projects.strict_con_conclusion'
        return if value then strict_pro else strict_con

      _getNonStrict = (value) ->
        nonstrict_pro = 'projects.nonstrict_pro_conclusion'
        nonstrict_con = 'projects.nonstrict_con_conclusion'
        return if value then nonstrict_pro else nonstrict_con

      key = if strict then _getStrict pro else _getNonStrict pro
      return $translate.instant key

    _getTooltip = ({scheme, id}) ->
      return if scheme then scheme.header.title else id

    _edit = () ->
      url = 'home.projects.project.arguments.argument.edit'
      $state.transitionTo url, $stateParams

    _openStatement = (sid) ->
      url = 'home.projects.project.statements.statement'
      params = pid: $stateParams.pid, db: $stateParams.db, sid: sid
      $state.transitionTo url, params

    argument.valueText = _getValueText argument
    $scope = extend $scope,
      argument: argument
      conclusion_text: _getConclusionText argument
      project: project
      edit: _edit
      remove: _remove
      openStatement: _openStatement
      tabModel: true
      tabMetadata: false
      showModel: _showModel
      showMetadata: _showMetadata
      isMapInitialized: editorService.isMapInitialized
      tooltipEdit: $translate.instant 'tooltip.argument.edit'
      tooltipRemove: $translate.instant 'tooltip.argument.remove'

    $state.$current.self.tooltip = _getTooltip argument

    return @

  .controller 'ArgumentEditCtrl', ($scope, $state, $stateParams, $translate,
    statements, argument, Argument, theory, breadcrumbService,
    editorService) ->
    _showModel = () ->
      $scope.tabModel = true
      $scope.tabMetadata = false

    _showMetadata = () ->
      $scope.tabModel = false
      $scope.tabMetadata = true

    _onSave = () ->
      argument.scheme = $scope.argument.scheme.id
      argument.scheme = "(#{argument.scheme})"
      argument.conclusion = $scope.argument.conclusion.id
      Argument.update($stateParams, argument).$promise.then((data) ->
        url = 'home.projects.project.arguments.argument'
        $state.transitionTo url, $stateParams, reload: true)

    _addPremise = () ->
      editorService.addPremise $scope.argument

    _deletePremise = (index) ->
      editorService.deletePremise $scope.argument, index

    _getScheme = (model) ->
      return editorService.getScheme model, theory.schemes

    _getStatementText = (model) ->
      return editorService.getStatementText model, statements

    _getStatement = (model) ->
      return editorService.getStatement model, statements

    _getSchemeId = ({scheme}) ->
      return scheme.slice 1, -1

    _getConclusionId = ({conclusion}) ->
      return conclusion.slice 1, -1

    if argument.conclusion?
      argument.conclusion = argument.conclusion.id

    if typeof argument.scheme is 'string'
      id = _getSchemeId argument
      argument.scheme = _getScheme id

    if typeof argument.conclusion is 'string'
      argument.conclusion = _getStatement argument.conclusion

    $scope = extend $scope,
      statements: statements
      title: $translate.instant 'projects.editargument'
      argument: argument
      theory: theory
      languages: editorService.getLanguages()
      getStatementText: _getStatementText
      editorOptions: editorService.getCodeMirrorOptions()
      addPremise: _addPremise
      deletePremise: _deletePremise
      onSave: _onSave
      onCancel: editorService.onCancel
      tabModel: true
      tabMetadata: false
      showModel: _showModel
      showMetadata: _showMetadata
      tooltipPremise: $translate.instant 'tooltip.premise'
      tooltipCancel: $translate.instant 'tooltip.cancel'
      tooltipSave: $translate.instant 'tooltip.argument.save'

    return @
