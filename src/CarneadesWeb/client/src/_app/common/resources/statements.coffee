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
  return angular.module("resources.statements", [
    "ngResource", 'app.helper'
  ])

  .factory "Statement", (urlService) ->
    url = '/projects/:pid/:db/statements/:sid'
    params = pid: "@pid", db: "@db", sid: "@sid"
    methods =
      'update':
        method: 'PUT'
    return urlService.$resource url, params, methods

  .factory "MultiStatementLoader", (Statement, $q) ->
    return (params) ->
      delay = $q.defer()
      Statement.query {}, params, ((statements) ->
        delay.resolve statements
      ), ->
        delay.reject "Unable to fetch statements"
      delay.promise

  .factory "StatementLoader", (Statement, $q) ->
    return (params) ->
      delay = $q.defer()
      Statement.get params, ((statement) ->
        delay.resolve statement
      ), ->
        delay.reject "Unable to fetch statement!"

      delay.promise
