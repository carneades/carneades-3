# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular'
  '../../../common/services/scroll'], (angular) ->
  angular.module('theory.controllers', [])
  .controller('TheoryCtrl', ($scope, $stateParams, $location, $anchorScroll, scroll, theory) ->
    $scope.showMetadatum = (k, v) ->
      v? and (k not in ['id', 'description', 'title'])

    $scope.stateParams = $stateParams
    $scope.lang = theory.lang
    $scope.section = theory

    if $stateParams.scrollTo?
      scroll.scrollTo $stateParams.scrollTo

  )
