# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular'
  'root'
  'classes'
  'angular-translate'
], (angular, cn) ->
  carneades = cn.carneades

  fixedEncodeURI = (str) ->
    encodeURI str
      .replace /%5B/g, '['
      .replace /%5D/g, ']'

  class DownloadController extends carneades.Controller
    @.$inject = [
      '$scope'
      '$attrs'
      '$http'
      ]

    constructor: (@scope, @attrs, @http) ->
      super()

    download: ->
      @scope.$emit 'download:start'
      @http
        .get @attrs.url
        .then (response) =>
          @scope.$emit 'download:finished', response.data

  svgDownloadDirective = ->
    fnLink = (scope, elm, attr) ->
      anchor = elm.children()[0]
      # when the download starts, disable the link
      scope.$on 'download:start', ->
        $(anchor).attr 'disabled', 'disabled'

      # when the download finishes, atatch teh data to the link.
      # Enable the link and change its appearance.
      scope.$on 'download:finished', (event, data) ->
        $(anchor).attr
          href: "data:image/svg+xml;charset=UTF-8,#{escape(data)}"
          download: attr.filename
          target: '_blank'
        .removeAttr 'disabled'
        .text 'Save'
        .removeClass 'btn-primary'
        .addClass 'btn-success'

        # also overwrite the download svg function to do nothing
        scope.download = ->

    restrict: 'E'
    template: '<a href="" class="btn btn-primary" ng-click="export.download()" download="test.svg">Download</a>'
    scope: true
    link: fnLink
    controller: 'ExportMapController'
    controllerAs: 'export'


  angular
    .module     'ui.carneades.share'
    .controller 'ExportMapController', DownloadController
    .directive  'svgDownload', svgDownloadDirective
