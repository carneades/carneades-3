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
  return angular.module("resources.nodes", [
    'ngResource', 'app.helper'
  ])

  .factory "Node", (urlService) ->
    url = '/projects/:pid/:db/nodes/:nid'
    params = pid: "@pid", db: "@db", nid: "@nid"
    return urlService.$resource url, params

  .factory "MultiNodeLoader", (Node, $q) ->
    return (params) ->
      delay = $q.defer()
      Node.query {}, params, ((nodes) ->
        delay.resolve nodes
      ), ->
        delay.reject "Unable to fetch nodes"

      delay.promise

  .factory "NodeLoader", (Node, $q) ->
    return (params) ->
      delay = $q.defer()
      Node.get params, ((node) ->
        delay.resolve node
      ), ->
        delay.reject "Unable to fetch argument!"

      delay.promise
