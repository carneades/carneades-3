# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ["angular", "angular-resource"], (angular) ->
  "use strict"
  services = angular.module("resources.statements", ["ngResource"])
  services.factory "Statement", ($resource, $location) ->
    $resource $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/:pid/:db/statements/:sid",
      pid: "@pid"
      db: "@db"
      sid: "@sid"

  services.factory "Statements", ($resource, $location) ->
    $resource $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/:pid/:db/statements/",
      pid: "@pid"
      db: "@db",
      'query':  {method:'GET', isArray:true}

  services.factory "StatementEdit", ($resource, $location) ->
    $resource $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/:pid/:db/statements/:sid?context=edit",
    {pid: "@pid", db: "@db", sid: "@sid"},
    {update: {method: 'PUT'}}

  services.factory "StatementCreate", ($resource, $location) ->
    $resource $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/:pid/:db/statements/",
    {pid: "@pid", db: "@db"}


  services.factory "MultiStatementLoader", (Statement, $q) ->
    return () ->
      delay = $q.defer()
      Statement.query ((statement) ->
        delay.resolve statement
      ), ->
        delay.reject "Unable to fetch nodes"

      delay.promise

  services.factory "StatementLoader", (Statement, $q) ->
    return (params) ->
      delay = $q.defer()
      Statement.get params, ((statement) ->
        delay.resolve statement
      ), ->
        delay.reject "Unable to fetch Statement!"

      delay.promise

  services
