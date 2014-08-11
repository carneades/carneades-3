# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  "angular",
  "angular-resource",
  '../services/app'
  ], (angular) ->
  "use strict"
  return angular.module("resources.map", [
    "ngResource", 'app.helper'

  ])

  .factory "Map", (urlService) ->
    url = "/projects/:pid/:db/map"
    params = pid: "@pid", db: "@db"
    methods = get:
      method: "GET"
      isArray: false

    return urlService.$resource url, params, methods

  .factory "MapLoader", (Map, $q) ->
    return (params) ->
      delay = $q.defer()
      Map.get params, ((args) ->
        delay.resolve args
      ), ->
        delay.reject "Unable to fetch nodes"

      delay.promise
