# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-translate'
], (angular) ->
  angular.module('ag.controllers', [
    'pascalprecht.translate'
  ])

  .controller('CreateAgCtrl', ($scope, $stateParams, ag) ->
    
    $scope.ag =
      name: "",
      header:
        description: {},
        title: ""

    $scope.onSave = ->
      console.log $scope.ag
      ag.save $stateParams, $scope.ag
  
  )
