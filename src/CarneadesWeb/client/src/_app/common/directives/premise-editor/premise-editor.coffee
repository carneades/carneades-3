# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-translate',
  '../../../common/directives/radio-buttons/radio-buttons',
], (angular) ->
  angular.module("directives.premiseEditor", ['pascalprecht.translate',
    'directives.radioButtons'])

  .directive("premiseEditor", ->
    restrict: "E"
    templateUrl: "common/directives/premise-editor/premise-editor.jade"
    scope:
      model: '=',
      statements: '=',
      onDelete: '&'
    controller: ($scope, $translate) ->
      $scope.onDeletePremise = ->
        console.log 'onDeletePremise'
        $scope.onDelete()
  )
