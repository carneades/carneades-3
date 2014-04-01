# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', 'angular-translate',
    '../../../common/directives/properties/properties',
    '../../../common/directives/metadata/metadata'], (angular) ->
  angular.module('statement.controllers', ['directives.properties',
    'directives.metadata'])
    .controller('StatementCtrl', ($scope, statement) ->
      $scope.statement = statement
      $scope.pid = $scope.$stateParams.pid
      $scope.db = $scope.$stateParams.db

      undefined
    )
