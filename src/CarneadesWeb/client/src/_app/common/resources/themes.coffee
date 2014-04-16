# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global define
define ["angular", "angular-resource"], (angular) ->
  "use strict"
  services = angular.module("resources.themes", ["ngResource"])
  services.factory "Theme", ($resource, $location) ->
    $resource $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/:pid/theme/:did",
      pid: "@pid"
      did: "@did"

  services.factory "ThemeLoader", (Theme, $q) ->
    (params) ->
      delay = $q.defer()
      Theme.get params, ((theme) ->
        delay.resolve theme
      ), ->
        delay.reject "Unable to fetch theme"

      delay.promise

  services
