# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', 'common/services/i18nNotifications', 'common/services/httpRequestTracker', 'common/resources/themes'], (angular) ->
  "use strict"
  angular.module('app.controllers', ['services.i18nNotifications', 'services.httpRequestTracker', 'resources.themes'])

  .controller('AppCtrl', () ->
    undefined
  )

  .controller 'HeaderCtrl', ($breadcrumb, $scope, $location, notifications, httpRequestTracker) ->
    $scope.hasPendingRequests = ->
      httpRequestTracker.hasPendingRequests()

    setNavigationState = () ->
      $scope.$navigationStates = $breadcrumb.getNavigationStates $scope.$state, $scope.$stateParams

    $scope.$on '$stateChangeSuccess', ->
      setNavigationState()

    undefined

  .directive('bcNavigation', () ->
    restrict: 'E'
    replace: 'true'
    #templateUrl: 'breadcrumb-navigation.jade'
    template: '<div class=\"my-fluid-container\" ng-controller="HeaderCtrl"><breadcrumb states="$navigationStates" style="style"></breadcrumb></div>'
    controller: ($scope, $element, $attrs, $stateParams) ->
      setTheme = () ->
        if $stateParams.pid is 'markos'
          $scope.style = 'simple'
        else
          $scope.style = 'emacs'

      $scope.$on '$stateChangeSuccess', ->
        setTheme()

      setTheme()
    )

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
