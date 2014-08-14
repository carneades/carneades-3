# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-capitalize-filter',
  'angular-translate'
], (angular) ->
  extend = (object, properties) ->
    for key, val of properties
      object[key] = val
    object

  addPremise = (scope) ->
    scope.argument.premises.push({role: "", implicit: false, positive: true})

  deletePremise = (scope, p) ->
    scope.argument.premises = (q for q in scope.argument.premises when p.role != q.role)

  onArgumentRetrieve = (scope) ->
    scope.schemeId = if scope.argument.scheme? and scope.argument.scheme != "" and scope.argument.scheme[0] == '('
      scope.argument.scheme.slice 1, -1
    else
      undefined

  onSchemeChange = (scope, newVal) ->
    if newVal == undefined
      return

    scope.argument.scheme = "(#{newVal})"

    scope.currentScheme = if scope.theory.schemes?
      (scope.theory.schemes.filter (s) ->
        s.id == newVal)[0]
    else
      undefined

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
    'angular-capitalize-filter',
    'directives.metadata'
  ])

  .controller 'ArgumentViewCtrl', ($scope, $state, $stateParams, $translate,
    argument, project) ->
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
        return if value then  nonstrict_pro else nonstrict_con

      key = if strict then _getStrict pro else _getNonStrict pro
      return $translate.instant key

    _isHeaderEmpty = ({header}) ->
      return (v for k,v of header when v?).length == 0

    _getTooltip = ({scheme, id}) ->
      return if scheme then scheme.header.title else id

    _edit = () ->
      url = 'home.projects.project.arguments.argument.edit'
      $state.transitionTo url, $stateParams

    argument.valueText = _getValueText argument
    $scope = extend $scope,
      argument: argument
      conclusion_text: _getConclusionText argument
      headerIsEmpty: _isHeaderEmpty argument
      project: project
      edit: _edit

    $state.$current.self.tooltip = _getTooltip argument

    return @

  .controller 'ArgumentEditCtrl', ($scope, $q, $stateParams, $translate, project,
    statements, argument, theory, projectInfo) ->

    $scope.title = $translate.instant 'projects.editargument'
    $scope.statements = statements
    $scope.argument = argument
    $q.all([argument]).then (data) ->
      onArgumentRetrieve $scope

    $scope.theory = theory.get
      pid: $stateParams.pid
      db: $stateParams.db
      tpid: projectInfo.getSchemesProject project
      tid: projectInfo.getSchemesName project

    $scope.$watch 'schemeId', (newVal) ->
      onSchemeChange $scope, newVal

    $scope.addPremise = () ->
      addPremise $scope

    $scope.deletePremise = (p) ->
      deletePremise $scope, p

    $scope.onSave = () ->
      console.log 'argument', $scope.argument
      #argumentedit.update $stateParams, $scope.argument

    return @
