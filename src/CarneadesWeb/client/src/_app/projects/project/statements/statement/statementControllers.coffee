# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular',
  'angular-translate',
  '../../../../common/directives/properties/properties',
  '../../../../common/directives/metadata/metadata',
  '../../../../common/directives/radio-buttons/radio-buttons',
  '../../../../common/directives/multilang-textarea/multilang-textarea',
], (angular) ->
  extend = (object, properties) ->
    for key, val of properties
      object[key] = val
    object

  return angular.module('statement.controllers', [
    'pascalprecht.translate',
    'directives.properties',
    'directives.metadata',
    'directives.radioButtons',
    'directives.multilangTextarea'
  ])

  .controller 'StatementViewCtrl', ($scope, $state, $stateParams, $translate, statement, project) ->
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

    _edit = () ->
      url = 'home.projects.project.statements.statement.edit'
      $state.transitionTo url, $stateParams


    statement.valueText = _getValueText statement
    $scope = extend $scope,
      statement: statement
      project: project
      headerIsEmpty: _isHeaderEmpty statement
      argumentName: _getArgumentName
      edit: _edit

    $state.$current.self.tooltip = _getTooltip statement

    return @

  .controller 'StatementEditCtrl', ($scope, $translate, $stateParams, statement) ->
    $scope.title = $translate.instant 'projects.editstatement'
    $scope.statement = statement
    $scope.standards = [
            { name: ($translate.instant 'projects.pe'), value: 'pe'},
            { name: ($translate.instant 'projects.dv'), value: 'dv'},
            { name: ($translate.instant 'projects.cce'), value: 'cce'},
            { name: ($translate.instant 'projects.brd'), value: 'brd'}
          ]

    $scope.onSave = () ->
      console.log 'statement', $scope.statement
      #statementedit.update $stateParams, statement

    return @
