# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ["angular", "angular-resource"], (angular) ->
  services = angular.module('resources.theories', ['ngResource'])

  services.factory 'Theory', ($resource, $location) ->
    $resource $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/:pid/theories/:tpid/:tid?translate=t",
      pid: '@pid',
      tpid: '@tpid',
      tid: '@tid'
