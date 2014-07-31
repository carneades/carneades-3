# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular'
], (angular) ->
  angular.module("directives.radioButtons", [])

  .directive("radioButtons", ->
    restrict: "E"
    templateUrl: "common/directives/radio-buttons/radio-buttons.jade"
    scope:
      model: '='
      options: '='
    controller: ($scope) ->
      $scope.activate = (option) ->
        $scope.model = option
  )
