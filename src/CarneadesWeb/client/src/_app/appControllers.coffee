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

  angular.module('app.controllers', ['services.i18nNotifications', 'services.httpRequestTracker', 'resources.themes'])

  .service 'themeService', () ->
    @setTheme = (scope, theme, fallback = 'default') ->
      scope.theme = theme || fallback

    return @

  .controller 'AppCtrl', ($scope, $stateParams, themeService) ->
    setTheme = ({pid}) ->
      $scope.style = if pid is 'markos' then 'simple' else 'emacs'
      #themeService.setTheme $scope, pid

    $scope.$on '$stateChangeSuccess', () ->
      setTheme $stateParams

    setTheme pid: $stateParams.pid || 'default'

    return @

  .controller 'MobSubnavController', ($scope, $state) ->
    update = () ->
      builder = (params...) ->
        create = (label, state, clazz) ->
          return {label: label, state: state, clazz: clazz}

        createCommands = ($state,states...) ->
          commands = []
          for state in states
            label = $state.get(state).label
            commands.push create label, state, undefined
          return commands
        return ($state) -> return createCommands($state, params...)

      commands = []
      if $state.current.data?.commands
        _commands = builder($state.current.data.commands...) $state
      $scope.commands = _commands
    $scope.$on '$stateChangeSuccess', -> update()

    update()

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
      if $state.current.data?.commands
        _commands = builder($state.current.data.commands...) $state
      $scope.commands = _commands

    $scope.$on '$stateChangeSuccess', -> update()

    update()

    return @

  .controller 'HeaderCtrl', ($scope, $location, $state, breadcrumbService) ->
    _openView = (name, params) -> $state.go name, params

    _getReversedNavigationStates = (states) ->
      return states.slice(0, states.length-1).reverse()

    $scope.$on '$stateChangeSuccess', ->
      breadcrumbService.updateStates()
      _hasHistory = breadcrumbService.isEmpty()
      data = $state.$current.self.data
      _isSubNavDisplayed = data? and data.commands.length > 0

      # In order to update the list passed to ng-repeat properly
      _navigatedStates = angular.copy breadcrumbService.getStates()
      _bcTop = _navigatedStates[_navigatedStates.length - 1]
      _subNavStatesReversed = _getReversedNavigationStates _navigatedStates

      $scope = extend $scope,
        navigatedStates: _navigatedStates
        bcTop: _bcTop
        subNavStatesReversed: _subNavStatesReversed
        navBcCollapsed: true
        navCollapsed: true
        hasHistory: _hasHistory
        isSubNavDisplayed: _isSubNavDisplayed

    $scope = extend $scope,
      openView: _openView
      hasHistory: false

    return @

  .directive 'cssInject', () ->
    restrict: 'E'
    replace: true
    template: '<link rel="stylesheet" ng-href="api/projects/{{theme}}/theme/css/{{theme}}.css" media="screen"/>'
    scope:
      theme: '=?'
    controller: ($scope, $stateParams, themeService) ->
      $scope.$on '$stateChangeSuccess', () ->
        theme = if $scope.theme then $scope.theme else $stateParams.pid
        themeService.setTheme $scope, theme

  .directive 'projectBanner', () ->
    restrict: 'E'
    replace: true
    template: '<div data-ng-include src="\'api/projects/\' + theme + \'/theme/html/banner.tpl\'"></div>'
    scope:
      theme: '=?'
    controller: ($scope, $stateParams, themeService) ->
      $scope.$on '$stateChangeSuccess', () ->
        theme = if $scope.theme then $scope.theme else $stateParams.pid
        themeService.setTheme $scope, theme

  .directive 'projectFooter', () ->
    restrict: 'E'
    replace: true
    template: '<div ng-include="\'api/projects/\' + theme + \'/theme/html/footer.tpl\'"></div>'
    scope:
      theme: '=?'
    controller: ($scope, $stateParams, themeService) ->
      $scope.$on '$stateChangeSuccess', () ->
        theme = if $scope.theme then $scope.theme else $stateParams.pid
        themeService.setTheme $scope, theme
