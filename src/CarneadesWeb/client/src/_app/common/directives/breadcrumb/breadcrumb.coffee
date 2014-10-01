# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  "angular",
  "angular-ui-router",
  "angular-bootstrap",
  "angular-translate"
], (angular) ->
  # Prefix state
  angular.module("ui.carneades.breadcrumb", [
    'pascalprecht.translate',
    'ui.bootstrap.collapse',
    "ui.router.state"
  ])

  .factory('$navigationStates', () ->
    class DS
      constructor: (@length) ->
        @data = new Array @length
      size: -> throw new Error('I am an abstract class!')
      push: (s) -> throw new Error('I am an abstract class!')
      pop: -> throw new Error('I am an abstract class!')
      peek: -> throw new Error('I am an abstract class!')

    class LimitedStack extends DS
      constructor: (length) ->
        super(length)
        @ptr = -1
      size: -> return @length
      push: (s) ->
        @ptr = @ptr + 1
        @ptr = @ptr % @length
        @data[@ptr] = s
      pop: () ->
        s = @data[@ptr]
        @ptr = if @ptr is 0 or @ptr is -1 then @ptr = @length else @ptr - 1
        return s
      peek: () -> return @data[@ptr]
      asArray: () ->
        arr = []
        i = 0
        p = @ptr
        while i isnt @length
          if @data[p] then arr.push(@data[p])
          p = if p is 0 or p is -1 then p = @length else p - 1
          i = i + 1
        return arr.reverse()
      isEmpty: () -> return @ptr is -1

    $navigationStates = (datastructure, size) ->
      if datastructure is 'LimitedStack' then return new LimitedStack(size)
      else throw new Error("Datastructure '#{datastructure}' not supported!")

    return $navigationStates
  )

  .factory('breadcrumbService', ($navigationStates, $state, $stateParams, $translate) ->
    options = {}
    navigationStates = $navigationStates('LimitedStack', 6)

    _render = (states) ->
      index = 0
      isActiveSet = false
      isActiveIndex = -1

      _fnSetIsActive = (s) ->
        s.isActive = ($state.$current.name is s.name)
        if s.isActive
          if isActiveSet
            states[isActiveIndex].isActive = false
            states[isActiveIndex].isLast = false
          isActiveIndex = index - 1
          isActiveSet = true

        return s

      _fnSetIsLast = (s, index) ->
        s.isLast = (index == states.length)
        return s

      return (_fnSetIsActive(_fnSetIsLast(s,++index)) for s in states)

    _build = () ->
      _translate = (stateName) ->
        label = $translate.instant($state.get(stateName).label)
        console.log label
        return label

      return {
        label: _translate $state.$current.name
        name: $state.$current.name
        params: angular.copy $stateParams
        tooltip: $state.$current.tooltip
      }

    @isEmpty = () -> return navigationStates.isEmpty()
    @updateStates = () -> navigationStates.push _build $state, $stateParams
    @getStates = () -> return navigationStates.asArray()
    @getRenderedStates = () -> return _render navigationStates.asArray(), $state
    @pop = () -> navigationStates.pop()
    @peek = () -> return navigationStates.peek()

    return @
  )

  .directive('breadcrumb', () ->
    restrict: 'EA'
    scope:
      states: '='
      style: '='
    replace: true
    templateUrl: 'common/directives/breadcrumb/breadcrumb.jade'
    controller: ($scope, $state) ->
      _index = -1
      $scope.setCommandView = (index) -> _index = index
      $scope.getActiveCommandView = () -> return _index

      return @
  )

  .directive('breadcrumbEntries', () ->
    restrict: 'E'
    replace: true
    transclude: true
    templateUrl: 'common/directives/breadcrumb/breadcrumb-entries.jade'
  )

  .directive('breadcrumbEntry', () ->
    restrict: 'E'
    replace: true
    scope:
      state: '='
      bcOpen: '&'
      index: '='
      style: '='
    templateUrl: 'common/directives/breadcrumb/breadcrumb-entry.jade'
    controller: ($scope, $state) ->
      $scope.openView = (name, params) -> $state.go name, params
      @getCssLevel = (style) ->
        cssClass = 'bc-level-simple'
        if style isnt 'markos'
          index = ($scope.index + 1) % 7
          cssClass = if index > 0 then "bc-level-" + index

        return cssClass

      return @
    link: (scope, element, attrs, ctrl) ->
      if scope.state.isActive
        element.addClass "active"
      else if scope.state.isLast
        element.addClass "last"
      else
        element.addClass ctrl.getCssLevel()
  )
