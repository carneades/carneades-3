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
  angular.module("app.states", ["ui.bootstrap.buttons"])
  .config(($stateProvider, $stateUtilProvider) ->
    helper = $stateUtilProvider.$get()
    states = [
      name: "home"
      label: "Carneades"
      url: "/"
      views:
        "css@":
          template: '<css-inject default-theme="default"></css-inject>'
        "banner@":
          template: "<project-banner></project-banner>"
        "footer@":
          template: "<project-footer></project-footer>"
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>Home</h1>"
        "subnav@":
          templateUrl: 'subnav.jade'
          resolve: helper.builder().add('commands', helper.cmdBuilder('home','home.projects','home.about','home.privacy','home.help','home.admin','home.signin')).build()
          controller: 'SubnavController'
    ,
      name: "home.about"
      label: "About"
      url: "about"
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>About</h1>"
        "subnav@":
          templateUrl: 'subnav.jade'
          resolve: helper.builder().add('commands', helper.cmdBuilder()).build()
          controller: 'SubnavController'
    ,
      name: "home.privacy"
      label: "Privacy"
      url: "privacy"
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>Privacy</h1>"
        "subnav@":
          templateUrl: 'subnav.jade'
          resolve: helper.builder().add('commands', helper.cmdBuilder()).build()
          controller: 'SubnavController'
    ,
      name: "home.help"
      label: "Help"
      url: "help"
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>Help</h1>"
        "subnav@":
          templateUrl: 'subnav.jade'
          resolve: helper.builder().add('commands', helper.cmdBuilder()).build()
          controller: 'SubnavController'
    ,
      name: "home.admin"
      label: "Admin"
      url: "admin"
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>Admin</h1>"
        "subnav@":
          templateUrl: 'subnav.jade'
          resolve: helper.builder().add('commands', helper.cmdBuilder()).build()
          controller: 'SubnavController'
    ,
      name: "home.signin"
      label: "Sign in"
      url: "signin"
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>Sign in</h1>"
        "subnav@":
          templateUrl: 'subnav.jade'
          resolve: helper.builder().add('commands', helper.cmdBuilder()).build()
          controller: 'SubnavController'
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
  )
  .controller('SubnavController', ($scope, commands) ->
    $scope.commands = commands)
