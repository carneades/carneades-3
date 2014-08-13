# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', 'common/services/i18nNotifications', 'common/services/httpRequestTracker', 'common/resources/themes'], (angular) ->
  "use strict"
  angular.module('app.controllers', ['services.i18nNotifications', 'services.httpRequestTracker', 'resources.themes'])

  .service('themeService', () ->
    @setTheme = (scope, theme, fallback = 'default') ->
      scope.theme = theme || fallback

    return @
  )

  .controller('AppCtrl', ($scope, $stateParams, themeService) ->
    setTheme = (pid) ->
      $scope.style = if pid is 'markos' then 'simple' else 'emacs'
      themeService.setTheme $scope, pid

    $scope.$on '$stateChangeSuccess', ->
      setTheme $stateParams.pid

    setTheme()

    return @
  )

  .controller('MobSubnavController', ($scope, $state) ->
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

      $scope.commands = builder($state.current.data.commands...) $state

    $scope.$on '$stateChangeSuccess', -> update()

    update()
  )

  .controller('SubnavController', ($scope, $state) ->
    update = () ->
      builder = (params...) ->
        create = (label, state, clazz) ->
          return {label: label, state: state, clazz: clazz}

        createCommands = ($state,states...) ->
          commands = []
          for state in states
            label = $state.get(state).label
            commands.push create label, state, undefined
            commands.push create '', undefined, 'divider'

          # since last item is a divider we must get rid off it
          if commands.length > 0 then commands.pop()
          return commands

        return ($state) -> return createCommands($state, params...)

      $scope.commands = builder($state.current.data.commands...) $state

    $scope.$on '$stateChangeSuccess', -> update()

    update()

    return @
  )

  .controller 'HeaderCtrl', ($scope, $location, $state, breadcrumbService) ->
    $scope.openView = (name, params) -> $state.go name, params
    $scope.hasHistory = false
    updateBreadcrumb = () ->
      breadcrumbService.updateStates()
      # In order to update the list passed to ng-repeat properly
      $scope.navigatedStates = angular.copy breadcrumbService.getStates()

      $scope.bcTop = $scope.navigatedStates[$scope.navigatedStates.length - 1]
      $scope.subNavStatesReversed = $scope.navigatedStates.slice(0, $scope.navigatedStates.length-1).reverse()
      $scope.navBcCollapsed = true

    $scope.$on '$stateChangeSuccess', ->
      $scope.navCollapsed = true
      $scope.hasHistory = breadcrumbService.isEmpty()
      $scope.isSubNavDisplayed = $state.$current.self.data? and $state.$current.self.data.commands.length > 0
      updateBreadcrumb()

    return @

  .directive 'cssInject', () ->
    restrict: 'E'
    replace: true
    template: '<link rel="stylesheet" ng-href="api/projects/{{theme}}/theme/css/{{theme}}.css" media="screen"/>'
    scope:
      theme: '=?'
    controller: ($scope, themeService) ->
      $scope.$watch 'theme', () -> themeService.setTheme $scope, $scope.theme

  .directive 'projectBanner', () ->
    restrict: 'E'
    replace: 'true'
    template: '<div ng-include="\'api/projects/\' + theme + \'/theme/html/banner.tpl\'"></div>'
    scope:
      theme: '=?'
    controller: ($scope, themeService) ->
      $scope.$watch 'theme', () -> themeService.setTheme $scope, $scope.theme

  .directive 'projectFooter', () ->
    restrict: 'E'
    replace: 'true'
    template: '<div ng-include="\'api/projects/\' + theme + \'/theme/html/footer.tpl\'"></div>'
    scope:
      theme: '=?'
    controller: ($scope, themeService) ->
      $scope.$watch 'theme', () -> themeService.setTheme $scope, $scope.theme
