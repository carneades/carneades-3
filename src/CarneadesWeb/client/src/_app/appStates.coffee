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
      label: "Carneades"
      url: "/"
      data:
        commands: ['home','home.projects','home.about','home.privacy','home.help','home.admin','home.signin']
      views:
        "css@":
          template: '<css-inject theme="$stateParams.pid"></css-inject>'
        "banner@":
          template: '<project-banner theme="$stateParams.pid"></project-banner>'
        "footer@":
          template: '<project-footer theme="$stateParams.pid"></project-footer>'
        "content@":
          templateUrl: 'home.jade'
        "subnav@":
          templateUrl: 'subnav.jade'
          controller: 'SubnavController'
        "mobsubnav@":
          templateUrl: 'subnav.jade'
          controller: 'MobSubnavController'
    ,
      name: "home.about"
      label: "About"
      parent: 'home'
      url: "about"
      data:
        commands: []
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>About</h1>"
    ,
      name: "home.privacy"
      label: "Privacy"
      url: "privacy"
      data:
        commands: []
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>Privacy</h1>"
    ,
      name: "home.help"
      label: "Help"
      url: "help"
      data:
        commands: []
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>Help</h1>"
    ,
      name: "home.admin"
      label: "Admin"
      url: "admin"
      data:
        commands: []
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>Admin</h1>"
    ,
      name: "home.signin"
      label: "Sign in"
      url: "signin"
      data:
        commands: []
      views:
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>Sign in</h1>"
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
  )
