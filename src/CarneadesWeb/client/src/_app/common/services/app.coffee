# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular'], (angular) ->

  angular.module('app.helper', [])

  .factory('urlService', ($resource, $location) ->
    url = [
      $location.protocol(),
      '://',
      $location.host(),
      ':',
      $location.port(),
      "/carneades/api"
    ].join ''

    @getURL = () ->
      return url

    @$resource = (path, params, methods) ->
      if methods? then return $resource url + path, params, methods
      return $resource url + path, params

    return @
  )
