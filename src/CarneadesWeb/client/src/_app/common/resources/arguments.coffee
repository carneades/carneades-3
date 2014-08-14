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
  return angular.module('resources.arguments', [
    'ngResource', 'app.helper'
  ])

  .factory "Argument", (urlService) ->
    url = '/projects/:pid/:db/arguments/:aid'
    params = pid: "@pid", db: "@db", aid: "@aid"
    return urlService.$resource url, params

  .factory "MultiArgumentLoader", (Argument, $q) ->
    return (params) ->
      delay = $q.defer()
      Argument.query {}, params, ((args) ->
        delay.resolve args
      ), ->
        delay.reject "Unable to fetch nodes"

      delay.promise

  .factory "ArgumentLoader", (Argument, $q) ->
    return (params) ->
      delay = $q.defer()
      Argument.get params, ((arg) ->
        delay.resolve arg
      ), ->
        delay.reject "Unable to fetch argument!"

      delay.promise
