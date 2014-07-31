# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular',
  'angular-translate',
  '../../../common/directives/properties/properties',
  '../../../common/directives/metadata/metadata'
], (angular) ->
  angular.module('statement.controllers', [
    'directives.properties',
    'directives.metadata',
    'pascalprecht.translate'
  ])

  .controller('StatementCtrl', ($scope, $translate, statement, project) ->
    $scope.statement = statement
    $scope.statement.valueText =
      if $scope.statement.value <= 0.25
        $translate.instant 'projects.statement.value.false'
      else if $scope.statement.value >= 0.75
        $translate.instant 'projects.statement.value.true'
      else
        $translate.instant 'projects.statement.value.uncertain'


    $scope.pid = $scope.$stateParams.pid
    $scope.db = $scope.$stateParams.db

    $scope.project = project

    $scope.headerIsEmpty = (v for k,v of statement.header when v?).length == 0

    $scope.argumentName = (arg, idx) ->
      if arg.scheme? and arg.scheme != ''
        arg.scheme.header.title
      else
        ($translate.instant "projects.argument") + " ##{idx+1}"

    $scope.$state.$current.self.tooltip = statement.text.substring(0, 100) + '...'

    undefined
  )
  .controller('StatementEditCtrl', ($scope, $translate, $stateParams, statementedit) ->
    
    $scope.title = $translate.instant 'projects.editstatement'
    $scope.statement = statementedit.get($stateParams)

    $scope.onSave = ->
      console.log 'onSave'

  )
