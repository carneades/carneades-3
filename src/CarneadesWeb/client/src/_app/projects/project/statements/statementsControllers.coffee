# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular',
  'angular-translate',
  '../../../common/directives/properties/properties',
  '../../../common/directives/metadata/metadata',
  '../../../common/directives/radio-buttons/radio-buttons',
  '../../../common/directives/multilang-textarea/multilang-textarea',
], (angular) ->
  angular.module('statements.controllers', [
    'pascalprecht.translate',
    'directives.properties',
    'directives.metadata',
    'directives.radioButtons',
    'directives.multilangTextarea'
  ])

  .controller('StatementNewCtrl', ($scope, $translate, $stateParams) ->
    $scope.title = $translate.instant 'projects.createstatement'
    $scope.statement =
      text: {en: "", fr: "", it: "", sp: "", nl: ""}
      header: {description: {en: "", fr: "", it: "", sp: "", nl: ""}}
      main: false
      standard: 'pe'

    $scope.standards = [
            { name: ($translate.instant 'projects.pe'), value: 'pe'},
            { name: ($translate.instant 'projects.dv'), value: 'dv'},
            { name: ($translate.instant 'projects.cce'), value: 'cce'},
            { name: ($translate.instant 'projects.brd'), value: 'brd'}
          ]

    $scope.onSave = () ->
      console.log 'on save', $scope.statement
      #statementcreate.save $stateParams, $scope.statement
  )
