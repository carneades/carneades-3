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
  angular.module("resources.metadata.references", [
    "ngResource", "app.helper"
  ])

  .factory "Reference", (urlService) ->
    url = '/projects/:pid/:db/metadata/references'
    params = pid: '@pid', db: '@db'
    return urlService.$resource url, params

  .factory "MultiReferenceLoader", (Reference, $q) ->
    return (params) ->
      delay = $q.defer()
      Reference.query params, ((reference) ->
        delay.resolve reference
      ), ->
        delay.reject "Unable to fetch reference"

      delay.promise
