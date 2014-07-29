# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-translate'
], (angular) ->
  angular.module("directives.metadataEditor", ['pascalprecht.translate'])

  .directive("metadataEditor", ->
    restrict: "E"
    templateUrl: "common/directives/metadata-editor/metadata-editor.jade"
    scope:
      model: '='
    controller: ($scope, $translate) ->
      console.log 'metadata controller'
      console.log $scope.model
      $scope.languages = ['en', 'de']
  )
