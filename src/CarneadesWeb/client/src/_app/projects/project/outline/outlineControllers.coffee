# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular',
], (angular) ->
  extend = (object, properties) ->
    for key, val of properties
      object[key] = val
    object

  return angular.module('outline.controllers', [])

  .controller 'OutlineRootCtrl', ($scope, $state, $stateParams, $q, project,
    tproject, scroll) ->

    $scope = extend $scope,
      viewLoading: true
      project: project
      scrollTo: scroll.scrollTo

    $q.all([project, tproject]).then (data) ->
      $scope.viewLoading = false

    return @

  .controller 'OutlineIssuesCtrl', ($scope, $q, issues) ->
    $scope = extend $scope,
      viewLoading: true
      issues: issues

    $q.all([issues]).then (data) ->
      $scope.viewLoading = false

    return @

  .controller 'OutlineReferencesCtrl', ($scope, $q, references) ->
    emptyReferences = (references) ->
      (v for k,v of references when v? and k != '$promise' and k != '$resolved')
      .length is 0

    $scope = extend $scope,
      viewLoading: true
      references: references
      hasReferences: not emptyReferences references

    $q.all([references]).then (data) ->
      $scope.viewLoading = false

    return @

  .controller 'OutlineOutlineCtrl', ($scope, $q, outline) ->
    $scope = extend $scope,
      viewLoading: true
      outline: outline

    $q.all([outline]).then (data) ->
      $scope.viewLoading = false

    return @
