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
  angular.module("app.states", [
    "ui.bootstrap.buttons"
  ])

  .config(($stateProvider) ->
    states = [
      name: "home"
      label: "home.label"
      url: "/"
      abstract: true
      onEnter: () ->
        console.log 'entered home state'
      onExit: () ->
        console.log 'left home state'
      views:
        "content@":
          templateUrl: 'home.jade'
        "header@":
          templateUrl: 'header.jade'
          controller: 'HeaderCtrl'
        "subnav@":
          template: '<page-navigation ng-show="commands.length > 0"><page-navigation-item cmd="c" ng-repeat="c in commands"></page-navigation-item></page-navigation>'
          controller: 'SubnavController'
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
  )
