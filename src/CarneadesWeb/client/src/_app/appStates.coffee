# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global define
define [
  "angular",
  "angular-bootstrap"
], (angular) ->
  "use strict"

  configure = ($stateProvider) ->
    states = [
      name: "home"
      label: "home.label"
      url: "/"
      abstract: true
      views:
        "content@":
          templateUrl: 'home.jade'
        "header@":
          templateUrl: 'header.jade'
          controller: 'HeaderCtrl'
          controllerAs: 'header'
        "subnav@":
          template: '<page-navigation ng-show="commands.length > 0"><page-navigation-item cmd="c" ng-repeat="c in commands"></page-navigation-item></page-navigation>'
          controller: 'SubnavController'
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state



  modules = [
    "ui.bootstrap.buttons"
    ]

  module = angular.module "app.states", modules

  module.config configure
