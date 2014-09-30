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
  return angular.module("resources.projects", [
    'ngResource', 'app.helper'
  ])

  .factory "Project", (urlService) ->
    url = '/projects/:pid'
    methods =
      'newArgumentGraph':
        method: 'POST'
        params:
          entity: 'ag'
      'update':
        method: 'PUT'
    return urlService.$resource url, pid: '@pid', methods

  .factory "MultiProjectLoader", (Project, $q) ->
    return (params) ->
      delay = $q.defer()
      Project.query {}, params, ((project) ->
        delay.resolve project
      ), ->
        delay.reject "Unable to fetch projects"

      delay.promise

  .factory "ProjectLoader", (Project, $q) ->
    return (params) ->
      delay = $q.defer()
      Project.get params, ((project) ->
        delay.resolve project
      ), ->
        delay.reject "Unable to fetch project " + params.id

      delay.promise
