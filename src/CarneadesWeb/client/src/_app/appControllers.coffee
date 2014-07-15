# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', 'common/services/i18nNotifications', 'common/services/httpRequestTracker', 'common/resources/themes'], (angular) ->
  "use strict"
  angular.module('app.controllers', ['services.i18nNotifications', 'services.httpRequestTracker', 'resources.themes'])

  .controller('AppCtrl', ($scope, $element, $attrs, $stateParams) ->
    setTheme = () ->
      if $stateParams.pid is 'markos'
        $scope.style = 'simple'
      else
        $scope.style = 'emacs'

    $scope.$on '$stateChangeSuccess', ->
      setTheme()

    setTheme()

    undefined
  )

  .controller('MobSubnavController', ($scope, $state) ->
    update = () ->
      builder = (params...) ->
        create = (label, state, clazz) ->
          return {label: label, state: state, clazz: clazz}
        command = ($state,state) ->
          return create $state.get(state).label, state, undefined

        createCommands = ($state,states...) ->
          commands = []
          for state in states
            commands.push command($state, state)

          return commands

        return ($state) ->
          return createCommands($state, params...)

      $scope.commands = builder($state.current.data.commands...) $state

    $scope.$on '$stateChangeSuccess', ->
      update()

    update()
  )

  .controller('SubnavController', ($scope, $state) ->
    update = () ->
      builder = (params...) ->
        create = (label, state, clazz) ->
          return {label: label, state: state, clazz: clazz}
        command = ($state,state) ->
          return create $state.get(state).label, state, undefined
        divider = () ->
          return create '', undefined, 'divider'

        createCommands = ($state,states...) ->
          commands = []
          for state in states
            commands.push command($state, state)
            commands.push divider()

          # since last item is a divider we must get rid off it
          if commands.length > 0 then commands.pop()
          return commands

        return ($state) ->
          return createCommands($state, params...)

      $scope.commands = builder($state.current.data.commands...) $state

    $scope.$on '$stateChangeSuccess', ->
      update()

    update()
  )

  .controller 'HeaderCtrl', ($breadcrumb, $scope, $location, $state, $stateParams) ->
    updateNavigatedStates = () ->
      $breadcrumb.updateNavigatedStates $state, $stateParams
      # In order to update the list passed to ng-repeat properly
      $scope.navigatedStates = angular.copy $breadcrumb.getNavigatedStates $state

    $scope.$on '$stateChangeSuccess', ->
      $scope.navCollapsed = true
      updateNavigatedStates()

    undefined

  .directive 'cssInject', ($compile, $stateParams) ->
    restrict: 'E'
    replace: true
    require: '?theme'
    template: '<link rel="stylesheet" ng-href="api/projects/{{theme}}/theme/css/{{theme}}.css" media="screen"/>'
    scope:
      theme: '=?'
    controller: ($scope) ->
      update = () ->
        $scope.theme = $scope.theme || 'default'

      $scope.$watch 'theme', () ->
        update()

      update()

  .directive 'projectBanner', ($compile) ->
    restrict: 'E'
    replace: 'true'
    scope:
      theme: '=?'
    template: '<div ng-include="\'api/projects/\' + theme + \'/theme/html/banner.tpl\'"></div>'
    controller: ($scope) ->
      update = () ->
        $scope.theme = $scope.theme || 'default'

      $scope.$watch 'theme', () ->
        update()

      update()

  .directive 'projectFooter', ($compile) ->
    restrict: 'E'
    replace: 'true'
    scope:
      theme: '=?'
    template: '<div ng-include="\'api/projects/\' + theme + \'/theme/html/footer.tpl\'"></div>'
    controller: ($scope) ->
      update = () ->
        $scope.theme = $scope.theme || 'default'

      $scope.$watch 'theme', () ->
        update()

      update()
