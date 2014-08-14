# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global define
define [
  "angular",
  "angular-resource",
  '../services/app'
  ], (angular) ->
  "use strict"
  return angular.module("resources.themes", [
    "ngResource", 'app.helper'
  ])

  .factory "Theme", (urlService) ->
    url = "/projects/:pid/theme/:did"
    params = pid: "@pid", did: "@did"
    return urlService.$resource url, params

  .factory "ThemeLoader", (Theme, $q) ->
    (params) ->
      delay = $q.defer()
      Theme.get params, ((theme) ->
        delay.resolve theme
      ), ->
        delay.reject "Unable to fetch theme"

      delay.promise
