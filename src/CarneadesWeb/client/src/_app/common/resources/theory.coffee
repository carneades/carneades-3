# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  "angular",
  "angular-resource",
  '../services/app'
  ], (angular) ->
  return angular.module('resources.theories', [
    'ngResource', 'app.helper'
  ])

  .factory 'Theory', (urlService) ->
    url = "/projects/:pid/theories/:tpid/:tid?translate=t"
    params = pid: '@pid', tpid: '@tpid', tid: '@tid'
    return urlService.$resource url, params
