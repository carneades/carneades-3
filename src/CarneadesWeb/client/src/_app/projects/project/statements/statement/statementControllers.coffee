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
  angular.module('statement.controllers', [
    'pascalprecht.translate',
    'directives.properties',
    'directives.metadata',
    'directives.radioButtons',
    'directives.multilangTextarea'
  ])

  .controller('StatementViewCtrl', ($scope, $state, $stateParams, $translate, statement, project) ->
    $scope.statement = statement
    $scope.project = project
    $scope.pid = $scope.$stateParams.pid
    $scope.db = $scope.$stateParams.db

    $scope.statement.valueText =
      if $scope.statement.value <= 0.25
        $translate.instant 'projects.statement.value.false'
      else if $scope.statement.value >= 0.75
        $translate.instant 'projects.statement.value.true'
      else
        $translate.instant 'projects.statement.value.uncertain'

    $scope.headerIsEmpty = (v for k,v of statement.header when v?).length == 0

    $scope.argumentName = (arg, idx) ->
        return if arg.scheme? and arg.scheme isnt ''
          arg.scheme.header.title
        else
          ($translate.instant "projects.argument") + " ##{idx+1}"

    tooltip = [statement.text.substring(0, 100), '...'].join ''
    $scope.$state.$current.self.tooltip = tooltip

    $scope.edit = () ->
      $state.transitionTo 'home.projects.project.statements.statement.edit', $stateParams

    return @
  )

  .controller('StatementEditCtrl', ($scope, $translate, $stateParams, statement) ->
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
  )
