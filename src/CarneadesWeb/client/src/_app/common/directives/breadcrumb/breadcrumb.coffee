# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ["angular", "angular-ui-router", "angular-bootstrap"], (angular) ->
  # Prefix state
  angular.module("ui.bootsrap.breadcrumb", ['ui.bootstrap.collapse', "ui.router.state"])
  .provider("$breadcrumb", () ->
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
      asArray: ->
        arr = []
        i = 0
        p = @ptr
        while i isnt @length
          if @data[p] then arr.push(@data[p])
          p = if p is 0 or p is -1 then p = @length else p - 1
          i = i + 1
        return arr.reverse()

    options = {}
    _navigationStates = new LimitedStack(6)
    position = 0

    setIndexOfItemClicked = (value) ->
      position = value

    getIndexOfItemClicked = () ->
      return position

    resetIndexOfItemClicked = () ->
      position = 0

    ##########################################################

    render = (states, nameOfCurrentState) ->
      index = 0
      fnIsActive = (s) ->
        s.isActive = (nameOfCurrentState is s.name)
        return s

      fnIsLast = (s, index) ->
        s.isLast = (index == states.length)
        return s

      return (fnIsActive(fnIsLast(s,++index)) for s in states)

    ##########################################################
    buildState = (state, stateParams) ->
      label: state.label
      name: state.name
      params: angular.copy stateParams
      tooltip: state.tooltip

    ##########################################################
    ##########################################################

    getNavigationStates = (state, stateParams) ->
      _navigationStates.push buildState state, stateParams
      return _navigationStates.asArray()

    $get: () ->
      getIndexOfItemClicked: () ->
        return getIndexOfItemClicked()

      setIndexOfItemClicked: (value) ->
        setIndexOfItemClicked value

      getNavigationStates: ($state, $stateParams) ->
        return render getNavigationStates($state.$current, $stateParams), $state.$current.name
  )
  .directive('breadcrumb', () ->
    restrict: 'EA'
    scope:
      states: '='
    replace: true
    templateUrl: 'directives/breadcrumb/breadcrumb.tpl.html'
    controller: ($scope, $state, $stateParams, $breadcrumb) ->
      getIndexOfActiveState = () ->
        index = 0
        idx = 0
        angular.forEach $scope.states, (s) ->
          if s.isActive then index = idx
          idx++
        return index

      _index = getIndexOfActiveState()

      getIndexByName = (states, name) ->
        index = 0
        idx = 0
        angular.forEach states, (s) ->
          if name == s.name then index = idx
          idx++
        return index

      $scope.setCommandView = (index) ->
        _index = index

      $scope.getActiveCommandView = () ->
        return _index
  )
  .directive('breadcrumbEntries', () ->
    restrict: 'E'
    replace: true
    transclude: true
    templateUrl: 'directives/breadcrumb/breadcrumb-entries.tpl.html'
  )
  .directive('breadcrumbEntry', () ->
    restrict: 'E'
    replace: true
    scope:
      state: '='
      bcOpen: '&'
      index: '='
    templateUrl: 'directives/breadcrumb/breadcrumb-entry.tpl.html'
    controller: ($scope, $element, $attrs, $state, $breadcrumb) ->
      $scope.openView = (name, params) ->
        $state.go name, params
    link: (scope, element, attrs) ->
      index = scope.index % 7
      scope.cssClass = if index > 0 then "bc-level-" + index
      if scope.state.isActive
        element.addClass "active"
      else if scope.state.isLast
        element.addClass "last"
      else
        element.addClass scope.cssClass

      #else angular.element(element).addClass "bcMinPanel"
  )
