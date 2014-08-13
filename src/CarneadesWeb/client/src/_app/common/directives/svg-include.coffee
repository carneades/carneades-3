# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  "angular"
], (angular) ->
  "use strict"
  angular.module("directives.svg.include", [])

  .directive 'svgInclude', ($compile) ->
    restrict: 'E'
    replace: true
    template: '<div></div>'
    require: "?ngModel"
    link: (scope, element, attrs, model) ->
      render = ->
        val = model.$modelValue
        if (val)
          e = $compile(val)(scope)
          element.append e
          element.css('width', "#{angular.element(e[2]).attr('width')}px")
          element.css('height', "#{angular.element(e[2]).attr('height')}px")
      if attrs['ngModel'] then scope.$watch(attrs['ngModel'], render)
