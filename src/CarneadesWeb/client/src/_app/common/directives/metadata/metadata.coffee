# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

## Displays a group of metadata
define ['angular', 'angular-translate'], (angular) ->
  angular.module("directives.metadata", ['pascalprecht.translate'])
  .directive("metadata", ->
    restrict: "E"
    replace: true
    templateUrl: "directives/metadata/metadata.tpl.html"
    scope:
      model: "=model"
  ).controller('MetadataCtrl', ["$scope", ($scope) ->
     $scope.getTranslateKey = (k) ->
      "projects.#{k}"
  ])