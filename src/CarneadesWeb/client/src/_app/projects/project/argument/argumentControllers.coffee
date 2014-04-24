# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', 'angular-translate'], (angular) ->
  angular.module('argument.controllers', ['pascalprecht.translate'])
    .controller('ArgumentCtrl', ($scope, argument, project, $translate) ->
      $scope.argument = argument
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
