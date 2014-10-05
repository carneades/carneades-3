# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular'
], (angular) ->
  return angular.module('outline.controllers', [])

  .controller 'OutlineRootCtrl', ($scope, $state, $stateParams,
    $translate, project, tproject, scroll, tpid) ->

    $stateParams.tpid = tpid
    $scope = angular.extend $scope,
      project: project
      scrollTo: scroll.scrollTo
      $stateParams: $stateParams
      tooltipEdit: $translate.instant 'tooltip.outline.edit'
      tooltipNewStatement: $translate.instant 'tooltip.statement.new'
      tooltipNewArgument: $translate.instant 'tooltip.argument.new'

    return @

  .controller 'OutlineIssuesCtrl', ($scope, issues) ->
    $scope = angular.extend $scope,
      issues: issues

    return @

  .controller 'OutlineReferencesCtrl', ($scope, references) ->
    emptyReferences = (references) ->
      (v for k,v of references when v? and k != '$promise' and k != '$resolved')
      .length is 0

    $scope = angular.extend $scope,
      references: references
      hasReferences: not emptyReferences references

    return @

  .controller 'OutlineOutlineCtrl', ($scope, outline) ->
    $scope = angular.extend $scope,
      outline: outline

    return @
