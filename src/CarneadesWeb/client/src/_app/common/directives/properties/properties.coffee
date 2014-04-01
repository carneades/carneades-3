# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

## Displays an ordered list of properties in table
define ['angular', 'angular-translate'], (angular) ->
  angular.module("directives.properties", ['pascalprecht.translate'])
  .directive("properties", ->
    restrict: "E"
    replace: true
    templateUrl: "directives/properties/properties.tpl.html"
    scope:
      keys: "=keys",
      model: "=model"
  )
