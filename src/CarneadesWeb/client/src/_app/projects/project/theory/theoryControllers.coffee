# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular',
  'angular-translate',
  '../../../common/services/scroll'
], (angular) ->
  angular.module('theory.controllers', [
    'pascalprecht.translate'
  ])

  .controller('TheoryCtrl', ($scope, $stateParams, scroll, theory, project) ->
    $scope.stateParams = $stateParams
    $scope.lang = theory.lang
    $scope.section = theory
    $scope.project = project

    if $stateParams.scrollTo?
      scroll.scrollTo $stateParams.scrollTo

  )

  .controller('SchemeCtrl', ($scope, $translate) ->
    scheme = $scope.scheme
    $scope.scheme = scheme
    $scope.conclusion_text =
      if scheme.strict and scheme.pro
        $translate.instant 'projects.strict_pro_conclusion'
      else if scheme.strict and not scheme.pro
        $translate.instant 'projects.strict_con_conclusion'
      else if not scheme.strict and scheme.pro
        $translate.instant 'projects.nonstrict_pro_conclusion'
      else if not scheme.strict and not scheme.pro
        $translate.instant 'projects.nonstrict_con_conclusion'
  )
