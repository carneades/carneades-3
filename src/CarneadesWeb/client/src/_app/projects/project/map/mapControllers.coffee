# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  "angular"
], (angular) ->
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

    .directive('bnMapMouseover', ($document, $parse) ->
      restrict: 'A'
      link: (scope, element, attrs) ->
        scopeExpression = attrs.bnMapMouseover
        invoker = $parse scopeExpression
        $document.on("mouseover", ( event ) ->
          scope.$apply(-> invoker(scope, { $event: event } ) )
          )
      )

    .directive('bnMapMouseout', ($document, $parse) ->
      restrict: 'A'
      link: (scope, element, attrs) ->
        scopeExpression = attrs.bnMapMouseout
        invoker = $parse scopeExpression
        $document.on("mouseout", ( event ) ->
          element = event.relatedTarget
          e = angular.element(element)

          if e[0]
            if e[0].nodeName
              if e[0].nodeName is 'tspan' or
              e[0].nodeName is 'rect'
                return
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
            $scope.$state.transitionTo("home.projects.project.statement", $scope.$stateParams)
          else if nid[0] != 'e'
            $scope.$stateParams.aid = nid.substr(2)
            $scope.$state.transitionTo("home.projects.project.argument", $scope.$stateParams)

        else
          nid = angular.element(element).parent().parent()
          if nid.attr('id')
            $scope.$stateParams.sid = nid.attr('id').substr(2)
            $scope.$state.transitionTo("home.projects.project.statement", $scope.$stateParams)

        return undefined

      $scope.handleMouseover = (event) ->
        element = event.target
        e = angular.element(element).parent()
        nid = angular.element(element).parent().attr('id')

        if nid
          if nid[0] is 's' or nid[0] isnt 'e'
            e.addClass "map-mouseover"
            if nid[0] is 's'
              item = e.find 'rect'
              item.css 'stroke', 'rgb(230,1,0)'
            # else if nid[0] is 'a'
            #   item = e.find 'circle'
            #   item.css 'stroke', 'rgb(230,1,0)'

        return undefined

      $scope.handleMouseout = (event) ->
        element = event.target
        e = angular.element(element).parent()
        nid = angular.element(element).parent().attr('id')
        if nid
          if nid[0] is 's' or nid[0] isnt 'e'
            if nid[0] is 's'
              item = e.find 'rect'
              item.css 'stroke', 'rgb(0,0,0)'
        #     # else if nid[0] is 'a'
        #     #   item = e.find 'circle'
        #     #   item.css 'stroke', 'rgb(0,0,0)'

        return undefined


      $scope.svg = map

    )
