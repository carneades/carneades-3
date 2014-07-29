# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

# argument graph
# 
define ["angular", "angular-resource"], (angular) ->
  services = angular.module("resources.ag", ["ngResource"])
  services.factory "Ag", ($resource, $location) ->
    $resource $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/:pid/?entity=ag", {pid: "@pid"}

  services
