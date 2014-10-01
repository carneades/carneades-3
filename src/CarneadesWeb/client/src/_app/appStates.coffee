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

    #   name: "home.about"
    #   label: "state.home.about.label"
    #   parent: 'home'
    #   url: "about"
    #   views:
    #     "content@":
    #       template: "<h1>About</h1>"
    # ,
    #   name: "home.privacy"
    #   label: "state.home.privacy.label"
    #   url: "privacy"
    #   views:
    #     "content@":
    #       template: "<h1>Privacy</h1>"
    # ,
    #   name: "home.help"
    #   label: "Help"
    #   url: "help"
    #   views:
    #     "content@":
    #       template: "<h1>Help</h1>"
    # ,
    #   name: "home.admin"
    #   label: "state.home.admin.label"
    #   url: "admin"
    #   views:
    #     "content@":
    #       template: "<h1>Admin</h1>"
    # ,
    #   name: "home.signin"
    #   label: "state.home.signin.label"
    #   url: "signin"
    #   views:
    #     "content@":
    #       template: "<h1>Sign in</h1>"
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
  )
