# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular'
  './outlineMainCtrl'
], (angular, OutlineMainCtrl) ->
  return angular.module('outline.controllers', [])

  .controller 'OutlineRootCtrl', OutlineMainCtrl

  .controller 'OutlineIssuesCtrl', ($scope, issues) ->
    $scope = angular.extend $scope,
      issues: issues

    return @

  .controller 'OutlineReferencesCtrl', ($scope, references) ->
    $scope = angular.extend $scope,
      references: references
      hasReferences: not (references.length == 0)

    return @

  .controller 'OutlineOutlineCtrl', ($scope, outline) ->
    $scope = angular.extend $scope,
      outline: outline

    console.log outline

    return @
