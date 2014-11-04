# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular',
  'spinjs',
  'angular-translate'
], (angular, Spinner) ->
  "use strict"
  metadataDirective = () ->
    return {
      restrict: "E"
      replace: true
      templateUrl: 'common/directives/share/metadata.jade'
      scope:
        model: "=model",
        skipped: "=skipped"
      controller: ($scope) ->
        console.log $scope.model
        @getTranslateKey = (k) -> return "projects.#{k}"
        @isHidden = ({skipped}, k, v) ->
          (not v?) or (skipped? and (skipped.indexOf k) != -1)
        return @
      link: (scope, element, attrs, metadataCtrl) ->
        scope.getTranslateKey = metadataCtrl.getTranslateKey
        scope.isHidden = (k, v) ->
          return metadataCtrl.isHidden scope, k, v
    }

  loaderDirective = () ->
    return {
      restrict: "A"
      replace: true
      transclude: true
      templateUrl: 'common/directives/share/loader.jade'
      scope:
        loading: "=myLoadingSpinner"
      link: (scope, element, attrs) ->
        opts = {
          lines: 13            # The number of lines to draw
          length: 20           # The length of each line
          width: 10            # The line thickness
          radius: 30           # The radius of the inner circle
          corners: 1           # Corner roundness (0..1)
          rotate: 0            # The rotation offset
          direction: 1         # 1: clockwise, -1: counterclockwise
          color: '#000'        # #rgb or #rrggbb or array of colors
          speed: 1             # Rounds per second
          trail: 60            # Afterglow percentage
          shadow: false        # Whether to render a shadow
          hwaccel: true        # Whether to use hardware acceleration
          className: 'spinner' # The CSS class to assign to the spinner
          zIndex: 2e9          # The z-index (defaults to 2000000000)
          top: '50%'           # Top position relative to parent
          left: '50%'          # Left position relative to parent
        }
        spinner = new Spinner(opts).spin()
        loadingContainer = angular.element(element[0].querySelector('.my-loading-spinner-container'))[0]
        loadingContainer.appendChild spinner.el
    }

  resizeDirective = ($window) ->
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
          return base - offset - 2

        scope.resizeHeight = () ->
          offset = document.getElementById('header-section').offsetHeight
          offset = offset + document.getElementById('subnav-section').offsetHeight
          return fnCalc newValue.h, offset

        scope.resizeHeightWithOffset = (offsetH = 0) ->
          return fnCalc newValue.h, offsetH

        scope.resizeWidthWithOffset = (offsetW = 0) ->
          return fnCalc newValue.w, offsetW
      , true)

      w.bind 'resize', () ->
        scope.$apply()

  angular.module('ui.carneades.share', [
    'pascalprecht.translate'
  ])

  .directive 'metadata', metadataDirective
  .directive 'myLoadingSpinner', loaderDirective
  .directive 'resize', resizeDirective
