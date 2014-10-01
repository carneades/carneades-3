# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular',
  'common/services/i18nNotifications',
  'common/services/httpRequestTracker',
  'common/resources/themes'
], (angular) ->
  "use strict"
  extend = (object, properties) ->
    for key, val of properties
      object[key] = val
    object

  angular.module('app.controllers', [
    'services.i18nNotifications',
    'services.httpRequestTracker',
    'resources.themes'
  ])

  .service 'themeService', () ->
    @setTheme = (scope, theme, fallback = 'default') ->
      scope.theme = theme || fallback
    return @

  .directive 'cssInject', () ->
    restrict: 'E'
    replace: true
    template: '<link rel="stylesheet" ng-href="api/projects/{{theme}}/theme/css/{{theme}}.css" media="screen"/>'
    scope:
      theme: '=?'
    controller: ($scope, $state, themeService) ->
      $scope.$watch 'theme', () ->
        theme = $scope.theme
        if $state.current.data and $state.current.data.theme
          theme = $state.current.data.theme
        themeService.setTheme $scope, theme

  .directive 'head', ($rootScope, $stateParams, $compile, $state) ->
    linker = (scope, elem) ->
      html = '<link rel="stylesheet" ng-repeat="(routeCtrl, cssUrl) in routeStyles" ng-href="{{cssUrl}}" />'
      elem.append($compile(html)(scope))
      scope.routeStyles = {}
      $rootScope.$on '$stateChangeStart', (event, toState, toParams, fromState, fromParams) ->
        scope.routeStyles = {}
        if fromState.data?.css
          unless Array.isArray fromState.data.css
            fromState.css = [fromState.data.css]
          angular.forEach fromState.data.css, (sheet) ->
            delete scope.routeStyles[sheet]

        if toState.data?.css
          unless Array.isArray toState.data.css
            toState.css = [toState.data.css]
          angular.forEach toState.data.css, (sheet) ->
            scope.routeStyles[sheet] = sheet

    return {
      restrict: 'E'
      link: linker
    }

  .controller 'SubnavController', ($scope, $state) ->

    update = () ->
      builder = (params...) ->
        create = (label, state, clazz) ->
          return {label: label, state: state, clazz: clazz}

        createCommands = ($state,states...) ->
          commands = []
          for state in states
            label = $state.get(state).label
            commands.push create label, state, undefined
            #commands.push create '', undefined, 'divider'

          # since last item is a divider we must get rid off it
          #if commands.length > 0 then commands.pop()
          return commands

        return ($state) -> return createCommands($state, params...)

      commands = []
      if $state.current.data and $state.current.data.commands
        _commands = builder($state.current.data.commands...) $state
      $scope.commands = _commands

    $scope.$on '$stateChangeSuccess', ->
      update()

    update()

    return @

  .controller 'HeaderCtrl', ($scope, $location, $state, breadcrumbService) ->
    $scope.viewLoading = true
    _openView = (name, params) -> $state.go name, params

    _getReversedNavigationStates = (states) ->
      return states.slice(0, states.length-1).reverse()

    update = () ->
      builder = (params...) ->
        create = (label, state, clazz) ->
          return {label: label, state: state, clazz: clazz}

        createCommands = ($state,states...) ->
          commands = []
          for state in states
            label = $state.get(state).label
            commands.push create label, state, undefined
            #commands.push create '', undefined, 'divider'

          # since last item is a divider we must get rid off it
          #if commands.length > 0 then commands.pop()
          return commands

        return ($state) -> return createCommands($state, params...)

      commands = []
      if $state.current.data and $state.current.data.commands
        _commands = builder($state.current.data.commands...) $state
      $scope.commands = _commands

    $scope.$on '$stateChangeSuccess', ->
      breadcrumbService.updateStates()
      update()

      data = $state.$current.self.data
      _isSubNavDisplayed = data and data.commands and data.commands.length > 0

      # In order to update the list passed to ng-repeat properly
      _navigatedStates = angular.copy breadcrumbService.getStates()
      _subNavStatesReversed = _getReversedNavigationStates _navigatedStates

      $scope = extend $scope,
        navigatedStates: _navigatedStates
        bcTop: breadcrumbService.peek()
        subNavStatesReversed: _subNavStatesReversed
        navBcCollapsed: true
        navCollapsed: true
        hasHistory: breadcrumbService.isEmpty()
        isSubNavDisplayed: _isSubNavDisplayed
        viewLoading: false

    $scope = extend $scope,
      openView: _openView
      hasHistory: false

    update()

    return @
