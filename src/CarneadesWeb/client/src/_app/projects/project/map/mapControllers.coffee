# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  "angular"
], (angular) ->
  "use strict"
  angular.module("map.controllers", [
    'ui.carneades.share'
  ])
  .directive('bnMapClick', ($document, $parse) ->
    restrict: 'A'
    link: (scope, element, attrs) ->
      scopeExpression = attrs.bnMapClick
      invoker = $parse scopeExpression
      $document.on("click", ( event ) ->
        scope.$apply(() -> invoker(scope, { $event: event } ) )
      )
  )

  .directive('bnMapMouseover', ($document, $parse) ->
    restrict: 'A'
    link: (scope, element, attrs) ->
      scopeExpression = attrs.bnMapMouseover
      invoker = $parse scopeExpression
      $document.on("mouseover", ( event ) ->
        scope.$apply(() -> invoker(scope, { $event: event } ) )
      )
  )

  .directive('bnMapMouseout', ($document, $parse) ->
    restrict: 'A'
    link: (scope, element, attrs) ->
      scopeExpression = attrs.bnMapMouseout
      invoker = $parse scopeExpression
      $document.on("mouseout", ( event ) ->
        scope.$apply(() -> invoker(scope, { $event: event } ) )
      )
  )

  .controller 'MapCtrl', ($scope, $stateParams, map) ->
    $scope.svg = map
    $scope.handleClick = (event) ->
      element = event.target
      if element.farthestViewportElement?.localName is 'svg'
        nid = angular.element(element).parent().attr 'id'
        if nid
          # rect (statement) or circle (argument)
          if nid[0] == 's'
            $stateParams.sid = nid.substr(2)
            url = 'home.projects.project.statements.statement'
            $scope.$state.transitionTo url, $stateParams
          else if nid[0] != 'e'
            $stateParams.aid = nid.substr(2)
            url = 'home.projects.project.arguments.argument'
            $scope.$state.transitionTo url, $stateParams
        else
          nid = angular.element(element).parent().parent()
          if nid.attr 'id'
            $stateParams.sid = nid.attr('id').substr(2)
            url = 'home.projects.project.statements.statement'
            $scope.$state.transitionTo url, $stateParams

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
        e = angular.element event.target
        e.css 'cursor', 'pointer'
        if (event.target.localName is 'rect') or
        (event.target.localName is 'circle')
          e.css 'stroke-width', '2px'
          e.css 'stroke-dasharray', '9, 5'
        else if event.target.localName is 'text'
          rect = e.parent().find 'rect'
          rect.css 'stroke-width', '2px'
          rect.css 'stroke-dasharray', '9, 5'
        else if event.target.localName is 'tspan'
          rect = e.parent().parent().find 'rect'
          rect.css 'stroke-width', '2px'
          rect.css 'stroke-dasharray', '9, 5'

      return undefined

    $scope.handleMouseout = (event) ->
      element = event.target
      if element.farthestViewportElement?.localName is 'svg'
        e = angular.element(element)
        if event.relatedTarget?.localName is 'svg'
          if (element.localName is 'rect') or (element.localName is 'circle')
            e.css 'stroke-width', '1px'
            e.css 'stroke-dasharray', '0'
          else if element.localName is 'text'
            rect = e.parent().find 'rect'
            rect.css 'stroke-width', '1px'
            rect.css 'stroke-dasharray', '0'
          else if element.localName is 'tspan'
            rect = e.parent().parent().find 'rect'
            rect.css 'stroke-width', '1px'
            rect.css 'stroke-dasharray', '0'
        else
          if (event.relatedTarget is null) or
          (event.target?.localName is 'tspan' and
          event.relatedTarget?.localName is 'tspan') or
          (event.target?.localName is 'rect' or
          event.originalTarget?.localName is 'rect') or
          (event.target?.localName is 'tspan' or
          event.originalTarget?.localName is 'tspan')
            rect = e.parent().parent().find 'rect'
            rect.css 'stroke-width', '1px'
            rect.css 'stroke-dasharray', '0'

      return undefined

    return @
