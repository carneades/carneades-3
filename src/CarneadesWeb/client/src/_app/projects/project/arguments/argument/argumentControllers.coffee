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

  .controller 'ArgumentViewCtrl', ($scope, $state, $stateParams, $translate,
    argument, project, editorService) ->
    console.log argument
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
      openStatement: _openStatement
      tabModel: true
      tabMetadata: false
      showModel: _showModel
      showMetadata: _showMetadata
      isMapInitialized: editorService.isMapInitialized
      tooltipEdit: $translate.instant 'tooltip.argument.edit'

    $state.$current.self.tooltip = _getTooltip argument

    return @

  .controller 'ArgumentEditCtrl', ($scope, $state, $stateParams, $translate,
    statements, argument, Argument, theory, breadcrumbService,
    editorService) ->
    console.log theory.schemes
    _showModel = () ->
      $scope.tabModel = true
      $scope.tabMetadata = false

    _showMetadata = () ->
      $scope.tabModel = false
      $scope.tabMetadata = true

    _onSave = () ->
      argument.scheme = "(#{$scope.argument.scheme})"
      Argument.update($stateParams, argument).$promise.then((data) ->
        url = 'home.projects.project.arguments.argument'
        $state.transitionTo url, $stateParams, reload: true)

    _addPremise = () ->
      editorService.addPremise $scope.argument

    _deletePremise = (p) ->
      editorService.deletePremise $scope.argument, p

    _getSchemeTitle = (model) ->
      return editorService.getSchemeTitle model, theory.schemes

    _getStatementText = (model) ->
      return editorService.getStatementText model, statements

    _getSchemeId = ({scheme}) ->
      return scheme.slice 1, -1

    if argument.conclusion?
      argument.conclusion = argument.conclusion.id

    if argument.scheme?
      argument.scheme = _getSchemeId argument

    $scope = extend $scope,
      statements: statements
      title: $translate.instant 'projects.editargument'
      argument: argument
      theory: theory
      languages: editorService.getLanguages()
      getSchemeTitle: _getSchemeTitle
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
