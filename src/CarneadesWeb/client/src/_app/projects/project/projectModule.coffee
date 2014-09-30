# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  './projectStates',
  './outline/outlineModule',
  './map/mapModule',
  './statements/statementsModule',
  './arguments/argumentsModule',
  './theory/theoryModule',
  './legalprofiles/legalprofilesModule'
], (angular) ->
  "use strict"
  angular.module 'project.module', [
    'project.states',
    'outline.module',
    'map.module',
    'statements.module',
    'arguments.module',
    'theory.module',
    'legalprofiles.module'
  ]
