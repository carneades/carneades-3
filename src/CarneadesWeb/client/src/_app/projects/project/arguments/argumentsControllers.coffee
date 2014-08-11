# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-capitalize-filter',
  'angular-translate',
  '../../../common/directives/metadata/metadata'
], (angular) ->
  angular.module('arguments.controllers', [
    'pascalprecht.translate',
    'directives.metadata',
    'angular-capitalize-filter'
  ])

  .controller('ArgumentNewCtrl', ($scope, $stateParams, $translate, project, theory, projectInfo, statements, argumentcreate) ->
    $scope.title = $translate.instant 'projects.createargument'
    $scope.statements = statements.query $stateParams

    $scope.argument =
      pro: true
      strict: false

    $scope.theory = theory.get {
      pid: $stateParams.pid,
      db: $stateParams.db,
      tpid: projectInfo.getSchemesProject(project),
      tid: projectInfo.getSchemesName(project)
    }

    $scope.$watch 'schemeId', (newVal) ->
      $scope.argument.scheme = "(#{newVal})"

    $scope.onSave = ->
      console.log 'argument', $scope.argument
      #argumentcreate.save $stateParams, $scope.argument
  )
