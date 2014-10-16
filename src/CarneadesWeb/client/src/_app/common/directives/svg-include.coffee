# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  "angular"
], (angular) ->
  "use strict"
  angular.module("directives.svg.include", [])

  .directive 'svgInclude', () ->
    restrict: 'E'
    replace: true
    template: '<div></div>'
    require: '?ngModel'
    link: (scope, element, attrs, model) ->
      render = ->
        val = model.$modelValue
        if val then element.append val

      if attrs['ngModel'] then scope.$watch(attrs['ngModel'], render)
