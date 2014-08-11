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
  return angular.module("resources.outline.issues", [
    "ngResource", 'app.helper'
  ])

  .factory "Issue", (urlService) ->
    url = '/projects/:pid/:db/outline/issues'
    params = pid: "@pid", db: "@db"
    return urlService.$resource url, params
    
  .factory "MultiIssueLoader", (Issue, $q) ->
    (params) ->
      delay = $q.defer()
      Issue.query params, ((issue) ->
        delay.resolve issue
      ), ->
        delay.reject "Unable to fetch issue"

      delay.promise
