# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global define
define ["angular", "angular-resource"], (angular) ->
  "use strict"
  services = angular.module("resources.metadata", ["ngResource"])
  services.factory "Metadata", ($resource, $location) ->
    $resource $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/:pid/:db/metadata/:mid",
      pid: "@pid"
      db: "@db"
      mid: "@mid"

  services.factory "MultiMetadataLoader", (Metadata, $q) ->
    ->
      delay = $q.defer()
      Metadata.query ((metadata) ->
        delay.resolve metadata
      ), ->
        delay.reject "Unable to fetch arguments"

      delay.promise

  services.factory "MetadataLoader", (Metadata, $q) ->
    (params) ->
      delay = $q.defer()
      Metadata.get params, ((metadata) ->
        delay.resolve metadata
      ), ->
        delay.reject "Unable to fetch metadata"

      delay.promise

  services
