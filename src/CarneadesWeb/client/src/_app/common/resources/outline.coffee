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
  return angular.module("resources.outline", [
    "ngResource", "app.helper"
  ])

  .factory "Outline", (urlService) ->
    url = '/projects/:pid/:db/outline'
    params = pid: '@pid', db: '@db'
    return urlService.$resource url, params

  .factory "MultiOutlineLoader", (Outline, $q) ->
    return (params) ->
      delay = $q.defer()
      Outline.query {}, params, ((outline) ->
        delay.resolve outline
      ), ->
        delay.reject "Unable to fetch outline"

      delay.promise
