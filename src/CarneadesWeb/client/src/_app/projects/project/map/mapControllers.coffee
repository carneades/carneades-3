# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ["angular"], (angular) ->
  "use strict"
  angular.module("map.controllers", [])
    .directive('bnMapClick', ($document, $parse) ->
      restrict: 'A'
      link: (scope, element, attrs) ->
        scopeExpression = attrs.bnMapClick
        invoker = $parse scopeExpression
        $document.on("click", ( event ) ->
          scope.$apply(-> invoker(scope, { $event: event } ) )
          )
      )
    .directive('map', ($compile) ->
      restrict: 'E'
      replace: true
      require: "?ngModel"
      link: (scope, element, attrs, model) ->
        render = ->
          val = model.$modelValue
          if (val)
            element.append val
            $compile(val)(scope)

        if attrs['ngModel']
          scope.$watch(attrs['ngModel'], render)

        )
    .controller('MapCtrl', ($scope, map) ->
      $scope.handleClick = (event) ->
        element = event.target
        nid = angular.element(element).parent().attr('id')
        if nid
          # rect (statement) or circle (argument)
          if nid[0] == 's'
            $scope.$stateParams.sid = nid.substr(2)
            $scope.$state.transitionTo("projects.project.statement", $scope.$stateParams)
          else
            $scope.$stateParams.aid = nid.substr(2)
            $scope.$state.transitionTo("projects.project.argument", $scope.$stateParams)

        else
          nid = angular.element(element).parent().parent().attr('id')
          if nid
            $scope.$stateParams.sid = nid.substr(2)
            $scope.$state.transitionTo("projects.project.statement", $scope.$stateParams)

      $scope.svg = map
    )