# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', 'angular-translate',
    '../../../common/directives/properties/properties',
    '../../../common/directives/metadata/metadata'], (angular) ->
  angular.module('statement.controllers', ['directives.properties',
    'directives.metadata', 'pascalprecht.translate'])
    .controller('StatementCtrl', ($scope, $translate, statement) ->
      $scope.statement = statement
      $scope.pid = $scope.$stateParams.pid
      $scope.db = $scope.$stateParams.db

      $scope.headerIsEmpty = (v for k,v of statement.header when v?).length == 0
      
      $scope.argumentName = (arg, idx) ->
        if arg.scheme? and arg.scheme != ''
          arg.scheme
        else
          ($translate.instant "projects.argument") + " ##{idx+1}"

      undefined
    )
