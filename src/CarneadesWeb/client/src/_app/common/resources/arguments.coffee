# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  "angular",
  "angular-resource"
], (angular) ->
  "use strict"
  services = angular.module('resources.arguments', ['ngResource'])

  services.factory "Argument", ($resource, $location) ->
    $resource $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/:pid/:db/arguments/:aid",
      pid: "@pid"
      db: "@db"
      aid: "@aid"

  services.factory 'ArgumentCreate', ($resource, $location) ->
    $resource $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/:pid/:db/arguments/",
      pid: "@pid"
      db: "@db"

  services.factory "MultiArgumentLoader", (Argument, $q) ->
    ->
      delay = $q.defer()
      Argument.query ((args) ->
        delay.resolve args
      ), ->
        delay.reject "Unable to fetch nodes"

      delay.promise

  services.factory "ArgumentLoader", (Argument, $q) ->
    (params) ->
      delay = $q.defer()
      Argument.get params, ((arg) ->
        delay.resolve arg
      ), ->
        delay.reject "Unable to fetch argument!"

      delay.promise

  services
