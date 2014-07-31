# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  "angular"
], (angular) ->
  "use strict"
  angular.module("directives.resize", [])

  .directive 'resize', ($window) ->
    return (scope, element, attr) ->
      w = angular.element $window
      scope.$watch(() ->
        return {
          'h': w.height()
          'w': w.width()
        }
      , (newValue, oldValue) ->
        scope.dimensions = [newValue.w,newValue.h]
        scope.windowHeight = newValue.h
        scope.windowWidth = newValue.w
        fnCalc = (base, offset) ->
          scope.$eval attr.notifier
          return base - offset

        scope.resizeHeightWithOffset = (offsetH = 0) ->
          return fnCalc newValue.h, offsetH

        scope.resizeWidthWithOffset = (offsetW = 0) ->
          return fnCalc newValue.w, offsetW
      , true)

      w.bind 'resize', () ->
        scope.$apply()
