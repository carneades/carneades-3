# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ["angular"], (angular) ->
  "use strict"
  angular.module("map.controllers", [])
    .directive('bnMapClick', ['$document', '$parse', ($document, $parse) ->
      restrict: 'A'
      link: (scope, element, attrs) ->
        scopeExpression = attrs.bnMapClick
        invoker = $parse scopeExpression
        $document.on("click", ( event ) ->
          scope.$apply(-> invoker(scope, { $event: event } ) )
          )
      ])
    .directive('map', ['$compile', ($compile) ->
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

        ])
    .controller('MapCtrl', ['$scope', "$stateParams", "$state", "map", ($scope, $stateParams, $state, map) ->
      $scope.handleClick = (event) ->
        element = event.target
        nid = angular.element(element).parent().attr('id')
        if nid
          # rect (statement) or circle (argument)
          if nid[0] == 's'
            $stateParams.sid = nid.substr(2)
            $state.transitionTo("projects.project.statement", $stateParams)
          else
            $stateParams.aid = nid.substr(2)
            $state.transitionTo("projects.project.argument", $stateParams)

        else
          nid = angular.element(element).parent().parent().attr('id')
          if nid
            $stateParams.sid = nid.substr(2)
            $state.transitionTo("projects.project.statement", $stateParams)

      $scope.svg = map
    ])
