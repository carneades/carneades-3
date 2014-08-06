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
  angular.module('argument.controllers', [
    'pascalprecht.translate',    
    'directives.metadata',    
    'angular-capitalize-filter'
  ])

  .controller('ArgumentCtrl', ($scope, argument, project, $translate) ->
    $scope.argument = argument
    $scope.argument.valueText =
      if $scope.argument.value <= 0.25
        $translate.instant 'projects.argument.value.unacceptable'
      else if $scope.argument.value >= 0.75
        $translate.instant 'projects.argument.value.acceptable'
      else
        $translate.instant 'projects.argument.value.unclear'

    $scope.pid = $scope.$stateParams.pid
    $scope.db = $scope.$stateParams.db

    $scope.headerIsEmpty = (v for k,v of argument.header when v?).length == 0

    $scope.project = project
    if argument.scheme
      $scope.$state.$current.self.tooltip = argument.scheme.header.title
    else
      $scope.$state.$current.self.tooltip = argument.id

    $scope.conclusion_text =
      if argument.strict and argument.pro
        $translate.instant 'projects.strict_pro_conclusion'
      else if argument.strict and not argument.pro
        $translate.instant 'projects.strict_con_conclusion'
      else if not argument.strict and argument.pro
        $translate.instant 'projects.nonstrict_pro_conclusion'
      else if not argument.strict and not argument.pro
        $translate.instant 'projects.nonstrict_con_conclusion'
  )
  .controller('ArgumentCreateCtrl', ($scope, $stateParams, $translate, project, theory, projectInfo) ->
    $scope.title = $translate.instant 'projects.createargument'
    $scope.argument =
      pro: true
      strict: false

    $stateParams.tpid = projectInfo.getSchemesProject(project)
    $stateParams.tid = projectInfo.getSchemesName(project)

    $scope.theory = theory.get $stateParams

    $scope.$watch 'schemeId', (newVal) ->
      $scope.argument.scheme = "(#{newVal})"
  )
