# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  "angular",
  "angular-resource",
  '../services/app'
  ], (angular) ->
  "use strict"
  return angular.module('resources.legalprofiles', [
      'ngResource', 'app.helper'
  ])

  .factory "Legalprofile", (urlService) ->
    url = '/projects/:pid/legalprofiles/:lpid'
    params = pid: "@pid", lpid: "@lpid"
    methods =
      'getRaw':
        method: 'GET'
        params:
          context: 'edit'
      'update':
        method: 'PUT'
    return urlService.$resource url, params, methods

  .factory "MultiLegalprofilesLoader", (Legalprofile, $q) ->
    return (params) ->
      delay = $q.defer()
      Legalprofile.query {}, params, ((legalprofiles) ->
        delay.resolve legalprofiles
      ), ->
        delay.reject "Unable to fetch legalprofiles"
      delay.promise


  .factory "LegalprofileLoader", (Legalprofile, $q) ->
    return (params) ->
      delay = $q.defer()
      Legalprofile.get params, ((legalprofile) ->
        delay.resolve legalprofile
        console.log 'Legalprofile is resolved:'
        console.log legalprofile
      ), ->
        delay.reject "Unable to fetch legalprofile!"

      delay.promise
