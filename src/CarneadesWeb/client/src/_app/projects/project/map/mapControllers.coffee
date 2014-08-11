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
          scope.$apply(-> invoker(scope, { $event: event } ) )
          )
      )

    .directive('map', ($parse, $compile) ->
      html = []
      html.push '<div style="'
      html.push 'margin:4px auto;height:{{ resizeHeightWithOffset(193) }}px;'
      html.push 'width:{{ resizeWidthWithOffset() }}px;'
      html.push 'white-space:pre-line;overflow:hidden;position:relative;">'
      html.push '<svg-include ng-model="svg"></svg-include></div>'
      return {
        restrict: 'A'
        replace: true
        template: html.join ''
        link: (scope, element, attrs) ->
          psOptions = [
            'wheelSpeed', 'wheelPropagation', 'minScrollbarLength',
            'useBothWheelAxes', 'useKeyboard', 'suppressScrollX',
            'suppressScrollY', 'scrollXMarginOffset','scrollYMarginOffset',
            'includePadding'
          ]

          options = {}
          for opt in psOptions
            do (opt) -> if attrs[opt]? then options[opt] = $parse(attrs[opt])()

          if attrs.refreshOnChange
            scope.$watchCollection attrs.refreshOnChange, () ->
              scope.$evalAsync () -> element.perfectScrollbar 'update'
          element.bind '$destroy', () -> element.perfectScrollbar 'destroy'
          element.perfectScrollbar options
      }
    )

    .controller('MapCtrl', ($scope, map) ->
      $scope.handleClick = (event) ->
        element = event.target
        if element.farthestViewportElement?.localName is 'svg'

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
        arrRelatedTargets = ['svg', 'rect', 'tspan', 'text', 'circle']
        target = event.target?.localName
        relatedTarget = event.relatedTarget?.localName
        if (target is 'tspan' and relatedTarget in arrRelatedTargets) or
        (target is 'text' and relatedTarget in ['rect', 'tspan']) or
        (target is 'rect' and relatedTarget in arrRelatedTargets) or
        (target is 'circle' and relatedTarget is 'svg') or
        (target is 'path' and relatedTarget is 'circle')
          e = angular.element(event.target)
          e.addClass "map-mouseover"
          if event.target.localName is 'rect'
            e.css 'stroke', 'rgb(230,1,0)'
          else if event.target.localName is 'text'
            e.parent().find('rect').css 'stroke', 'rgb(230,1,0)'
          else if event.target.localName is 'tspan'
            e.parent().parent().find('rect').css 'stroke', 'rgb(230,1,0)'

        return undefined

      $scope.handleMouseout = (event) ->
        element = event.target
        if element.farthestViewportElement?.localName is 'svg'
          if event.relatedTarget?.localName is 'svg'
            if element.localName is 'rect'
              angular.element(element).css 'stroke', 'rgb(0,0,0)'
            else if element.localName is 'text'
              angular.element(element).parent().find('rect').css 'stroke', 'rgb(0,0,0)'
            else if element.localName is 'tspan'
              angular.element(element).parent().parent().find('rect').css 'stroke', 'rgb(0,0,0)'
          else
            if (event.relatedTarget is null) or
            (event.target?.localName is 'tspan' and event.relatedTarget?.localName is 'tspan') or
            (event.target?.localName is 'rect' or event.originalTarget?.localName is 'rect')or
            (event.target?.localName is 'tspan' or event.originalTarget?.localName is 'tspan')
              angular.element(element).parent().parent().find('rect').css 'stroke', 'rgb(0,0,0)'

        return undefined

      $scope.svg = map
    )
