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
  return angular.module('resources.arguments', [
    'ngResource', 'app.helper'
  ])

  # Argument {
  #   strict: boolean
  #   conclusion: {
  #     text: string
  #     positive: boolean
  #     id: string
  #   }
  #
  #  dependents: array
  #  value: float
  #  header: {
  #    description: string
  #    format: null
  #    date: null
  #    creator: null
  #    publisher: null
  #    key: null
  #    coverage: null
  #    type: null
  #    source: null
  #    title: null
  #    rights: null
  #    contributor: null
  #    language: null
  #    identifier: null
  #    relation: null
  #    subject: null
  #  }
  #
  #  pro: boolean
  #  weight: float
  #  rebuttals: array
  #  id: string
  #  exceptions: array
  #  scheme: {
  #    formalized: boolean
  #    header: {
  #      title: string
  #    }
  #    id: string
  #  }
  #  premises: array
  #
  #  undercutters: array
  # }

  .factory "Argument", (urlService) ->
    url = '/projects/:pid/:db/arguments/:aid'
    params = pid: "@pid", db: "@db", aid: "@aid"
    methods =
      'getRaw':
        method: 'GET'
        params:
          context: 'edit'
      'update':
        method: 'PUT'
    return urlService.$resource url, params, methods

  .factory "MultiArgumentLoader", (Argument, $q) ->
    return (params) ->
      delay = $q.defer()
      Argument.query {}, params, ((args) ->
        delay.resolve args
      ), ->
        delay.reject "Unable to fetch nodes"

      delay.promise

  .factory "ArgumentLoader", (Argument, $q) ->
    return (params) ->
      delay = $q.defer()
      Argument.get params, ((arg) ->
        delay.resolve arg
      ), ->
        delay.reject "Unable to fetch argument!"

      delay.promise
