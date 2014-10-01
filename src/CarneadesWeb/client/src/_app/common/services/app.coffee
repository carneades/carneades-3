# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular'], (angular) ->

  urlService = ($resource, $location) ->
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


  $cssProvider = () ->
    _storage = []

    _getIndexOf = (id) ->
      idx = 0
      angular.forEach _storage, (item) ->
        if item.id is id then return
        idx = idx + 1
      return idx;

    _add = (id, val) ->
      _storage.push id: id, val: val

    _remove = (id) ->
      idx = _getIndexOf id
      _storage.slice idx, 1

    _contains = (id) ->
      bFound = false
      angular.forEach _storage, (item) ->
        if item.id is id
          bFound = true
          return;
      return bFound;

    return {
      add: _add
      $get: () ->
        get: () -> return _storage
        clearAll: () -> _storage = []
        contains: _contains
        remove: _remove
        add: _add
    }

  angular.module('app.helper', [])
  .factory 'urlService', urlService
  .provider '$css', $cssProvider

