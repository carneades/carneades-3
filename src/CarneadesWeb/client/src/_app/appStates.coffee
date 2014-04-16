# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global define
define ["angular", "angular-bootstrap"], (angular) ->
  "use strict"
  angular.module("app.states", ["ui.bootstrap.buttons"]).config ['$stateProvider', ($stateProvider) ->
    states = [
      name: "home"
      label: "Carneades"
      url: "/"
      views:
        "banner@":
          template: "<project-banner></project-banner>"
        "nav@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>Home</h1>"
        "page-nav@":
          template: '<page-navigation><page-navigation-item cmd=\"c\" ng-repeat=\"c in $state.$current.commands\"></page-navigation-item></page-navigation>'
      commands: [
        label: "Home"
        state: "home"
      ,
        label: "Projects"
        state: "home.projects"
      ,
        label: "About"
        state: "home.about"
      ,
        label: "Privacy"
        state: "home.privacy"
      ,
        label: "Help"
        state: "home.help"
      ,
        label: "Admin"
        state: "home.admin"
      ,
        label: "Sign in"
        state: "home.signin"
      ]
    ,
      name: "home.about"
      label: "About"
      url: "about"
      views:
        "@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>About</h1>"
    ,
      name: "home.privacy"
      label: "Privacy"
      url: "privacy"
      views:
        "@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>Privacy</h1>"
    ,
      name: "home.help"
      label: "Help"
      url: "help"
      views:
        "@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>Help</h1>"
    ,
      name: "home.admin"
      label: "Admin"
      url: "admin"
      views:
        "@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>Admin</h1>"
    ,
      name: "home.signin"
      label: "Sign in"
      url: "signin"
      views:
        "@":
          template: "<bc-navigation></bc-navigation>"
        "content@":
          template: "<h1>Sign in</h1>"
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
    ]
