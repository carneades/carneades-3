# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ["angular", "angular-resource"], (angular) ->
  "use strict"
  services = angular.module("resources.map", ["ngResource"])
  services.factory "Map", ($resource, $location) ->
    $resource $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/:pid/:db/map",
      pid: "@pid"
      db: "@db"
    ,
      get:
        method: "GET"
        isArray: false

  services.factory "MapLoader", (Map, $q) ->
    (params) ->
      delay = $q.defer()
      Map.get params, ((args) ->
        delay.resolve args
      ), ->
        delay.reject "Unable to fetch nodes"

      delay.promise
