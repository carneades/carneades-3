# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

# Displays an ordered list of properties in a table
# If the properties is a scheme, it is linked to its page
define [
  'angular',
  'angular-translate',
  '../../services/projectInfo'
], (angular) ->
  angular.module('directives.properties', [
    'pascalprecht.translate',
    'services.projectInfo'
  ])

  .directive("properties", ->
    restrict: "E"
    templateUrl: "common/directives/properties/properties.jade"
    scope:
      keys: "=",
      model: "="
    controller: ($scope, $translate, projectInfo) ->
      $scope.project = $scope.$parent.project
      $scope.pid = $scope.$parent.pid
      $scope.db = $scope.$parent.db

      $scope.getSchemesProject = (project) ->
        schemes = project.schemes
        res = schemes.split '/'
        if res.length is 1 then project.id else res[0]

      $scope.getSchemesName = (project) ->
        res = project.schemes.split '/'
        if res.length is 1 then res[0] else res[1]

      $scope.schemesProject = projectInfo.getSchemesProject($scope.project)
      $scope.schemesName = projectInfo.getSchemesName($scope.project)

      $scope.typeOfDisplay = (k, v) ->
        if k is 'scheme' and v.formalized
          'formalizedScheme'
        else if k is 'scheme' and not v.formalized
          'unformalizedScheme'
        else if k is 'standard'
          'standard'
        else
          'default'

      $scope.standardName = (s) ->
        $translate.instant "projects.#{s}"
  )
