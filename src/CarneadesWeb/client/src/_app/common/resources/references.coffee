# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global define
define ["angular", "angular-resource"], (angular) ->
  services = angular.module("resources.metadata.references", ["ngResource"])
  services.factory "Reference", ($resource, $location) ->
    $resource $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/:pid/:db/metadata/references",
      pid: "@pid"
      db: "@db"

  services.factory "MultiReferenceLoader", (Reference, $q) ->
    (params) ->
      delay = $q.defer()
      Reference.query params, ((reference) ->
        delay.resolve reference
      ), ->
        delay.reject "Unable to fetch reference"

      delay.promise

  return services
