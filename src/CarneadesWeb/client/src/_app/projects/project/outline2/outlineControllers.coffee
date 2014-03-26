# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular'], (angular) ->
  "use strict"
  angular.module('outline2.controllers', [])
  .controller('OutlineCtrl', ['$scope', '$stateParams', '$location', '$anchorScroll', 'project', 'node', 'metadata', ($scope, $stateParams, $location, $anchorScroll, project, node, metadata) ->
    $scope.args = node
    $scope.project = project
    $scope.meta = metadata
    $scope.pid = $stateParams.pid
    $scope.db = $stateParams.db

    $scope.gotoSection = (section) ->
      $location.hash section
      $anchorScroll()
      setTimeout ->
        window.scrollTo(window.pageXOffset, window.pageYOffset - 90)
      , 200
    ])
