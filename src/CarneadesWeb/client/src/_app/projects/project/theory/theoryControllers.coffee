# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular'], (angular) ->
  angular.module('theory.controllers', [])
  .controller('TheoryCtrl', ($scope, $location, $anchorScroll, theory) ->
    $scope.showMetadatum = (k, v) ->
      v? and (k not in ['id', 'description', 'title'])

    $scope.gotoSection = (section) ->
      setTimeout ->
        window.scrollTo(window.pageXOffset, window.pageYOffset - 90)

        old = $location.hash()
        $location.hash section
        $anchorScroll()
        $location.hash old

      , 200



    $scope.stateParams = $scope.$stateParams
    $scope.lang = theory.lang
    $scope.section = theory

    if $stateParams.scrollTo?
      $scope.gotoSection $scope.$stateParams.scrollTo

  )
