# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global define
define ["angular", "angular-resource"], (angular) ->
  "use strict"
  services = angular.module("resources.outline", ["ngResource"])
  services.factory "Outline", ($resource, $location) ->
    $resource $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/:pid/:db/outline",
      pid: "@pid"
      db: "@db"

  services.factory "MultiOutlineLoader", (Outline, $q) ->
    (params) ->
      delay = $q.defer()
      Outline.query params, ((outline) ->
        delay.resolve outline
      ), ->
        delay.reject "Unable to fetch outline"

      delay.promise

  return services
