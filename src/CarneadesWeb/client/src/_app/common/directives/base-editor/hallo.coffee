# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

## Displays a group of metadata
define [
  'angular'
], (angular) ->
  return angular.module("directives.editor", [])

  .directive('hallo', ($parse) ->
      restrict: 'AC'
      scope: true
      compile: (tElement, tAttrs) ->
        return (scope, elm, attr) ->
          params = scope.$eval attr.hallo
          bound = angular.isDefined attr.ngModel
          contents = if bound then scope.$eval(attr.ngModel) else elm.html()
          elm.hallo(params).html(contents).addClass('editable')

          if bound?
            model = $parse attr.ngModel
            elm.bind 'hallomodified', (event, data) ->
              scope.$apply(() ->
                contents = data.content
                model.assign scope, contents)
  )
