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
  return angular.module("resources.metadata", [
    "ngResource", 'app.helper'
  ])

  .factory "Metadata", ($resource, $location) ->
    url = '/projects/:pid/:db/metadata/:mid'
    params = pid: "@pid", db: "@db", mid: "@mid"
    return urlService.$resource url, params

  .factory "MultiMetadataLoader", (Metadata, $q) ->
    return (params) ->
      delay = $q.defer()
      Metadata.query params, ((metadata) ->
        delay.resolve metadata
      ), ->
        delay.reject "Unable to fetch arguments"

      delay.promise

  .factory "MetadataLoader", (Metadata, $q) ->
    return (params) ->
      delay = $q.defer()
      Metadata.get params, ((metadata) ->
        delay.resolve metadata
      ), ->
        delay.reject "Unable to fetch metadata"

      delay.promise
