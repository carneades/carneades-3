# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-translate'
], (angular) ->
  angular.module("directives.multilangTextarea", ['pascalprecht.translate'])

  .directive("multilangTextarea", ->
    restrict: "E"
    templateUrl: "common/directives/multilang-textarea/multilang-textarea.jade"
    scope:
      model: '='
    controller: ($scope, $translate) ->
      $scope.languages = [['en', 'En'],
        ['de', 'De'],
        ['fr', 'Fr'],
        ['it', 'It'],
        ['sp', 'Sp'],
        ['nl', 'Nl']]
  )
