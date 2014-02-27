# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global define
define ["angular"], (angular) ->
  "use strict"
  angular.module("app.states", []).config ['$stateProvider', ($stateProvider) ->
    states = [
      name: "home"
      label: "Carneades"
      url: "/"
      template: "<h1>Hello World!</h1>"
      commands: [
        label: "Home"
        state: "home"
      ,
        label: "Projects"
        state: "projects"
      ,
        label: "About"
        state: "about"
      ,
        label: "Privacy"
        state: "privacy"
      ,
        label: "Help"
        state: "help"
      ,
        label: "Admin"
        state: "admin"
      ,
        label: "Sign in"
        state: "signin"
      ]
    ,
      name: "about"
      label: "About"
      url: "/about"
      template: "<h1>About</h1>"
    ,
      name: "privacy"
      label: "Privacy"
      url: "/privacy"
      template: "<h1>Privacy</h1>"
    ,
      name: "help"
      label: "Help"
      url: "/help"
      template: "<h1>Help</h1>"
    ,
      name: "admin"
      label: "Admin"
      url: "/admin"
      template: "<h1>Admin</h1>"
    ,
      name: "signin"
      label: "Sign in"
      url: "/signin"
      template: "<h1>Sign in</h1>"
    ]
    
    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
    ]
