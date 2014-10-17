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

  .factory "Metadata", (urlService) ->
    url = '/projects/:pid/:db/metadata/:mid'
    params = pid: "@pid", db: "@db", mid: "@mid"
    methods =
      'getRaw':
        method: 'GET'
        params:
          context: 'edit'
      'update':
        method: 'PUT'
    return urlService.$resource url, params, methods

  .factory "MultiMetadataLoader", (Metadata, $q) ->
    return (params) ->
      delay = $q.defer()
      Metadata.query {}, params, ((metadata) ->
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

  .factory "MetadataRawLoader", (Metadata, $q) ->
    return (params) ->
      delay = $q.defer()
      Metadata.getRaw params, ((metadata) ->
        delay.resolve metadata
      ), ->
        delay.reject "Unable to fetch metadata " + metadata.id

      delay.promise
