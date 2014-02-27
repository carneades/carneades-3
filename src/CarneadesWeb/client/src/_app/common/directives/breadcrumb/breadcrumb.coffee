# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ["angular", "angular-ui-router", "angular-local-storage", "angular-bootstrap"], (angular) ->
  "use strict"

  # Prefix state
  angular.module("ui.bootsrap.breadcrumb", ['ui.bootstrap.collapse', "ui.router.state", "LocalStorageModule"])

  .constant('breadcrumbConfig',
      closeOthers: true
  )

  .config(['localStorageServiceProvider', (localStorageServiceProvider) ->
    localStorageServiceProvider.setPrefix('breadcrumb')
  ])

  .provider("$breadcrumb", () ->
    options = {}
    storageService = {}

    setStorageService = (service) ->
      storageService = service

    getVisitedStates = ->
      visited = storageService.get 'visited'
      return visited ?= []

    storeVisitedStates = (states) -> storageService.set 'visited', states

    $get: ['localStorageService', (localStorageService) ->
      getVisitedStates: ->
        setStorageService localStorageService
        return  getVisitedStates()

      storeVisitedStates: (states) ->
        storeVisitedStates states
    ]
  )

  .controller('BreadcrumbController', ['$scope', '$compile', '$attrs', 'breadcrumbConfig', ($scope, $compile, $attrs, breadcrumbConfig) ->
    @groups = []

    @closeOthers = (openGroup) ->
      closeOthers = if angular.isDefined $attrs.closeOthers then $scope.$eval $attrs.closeOthers else breadcrumbConfig.closeOthers
      if closeOthers
        angular.forEach @groups, (group) ->
          if group != openGroup
            group.isOpen = false
            group.isHover = false

    @addGroup = (groupScope) ->
      that = @
      @groups.push groupScope
      groupScope.$on '$destroy', (event) ->
        that.removeGroup groupScope

      undefined

    @setCommands = (groupScope) ->
      $scope.commands = groupScope.commands

    @getIndexOfGroup = (group) ->
      @groups.indexOf group

    @removeGroup = (group) ->
      index = @groups.indexOf group
      if index != -1 then @groups.splice @groups.indexOf(group), 1

      undefined

    undefined
  ])

  .directive('breadcrumb', () ->
    restrict: 'EA'
    controller: 'BreadcrumbController'
    transclude: true
    replace: true
    templateUrl: 'directives/breadcrumb/breadcrumb.tpl.html'
  )

  .directive('breadcrumbGroup', ['$parse', ($parse) ->
    require: '^breadcrumb'
    restrict: 'EA'
    transclude: true
    replace: true
    templateUrl: 'directives/breadcrumb/breadcrumb-entry.tpl.html'
    scope:
      label: '@'
      commands: '='
      isActive: '='
      isLast: '='
    controller: () ->
      @setLabel = (element) ->
        @label = element
      @setCommands = (element) ->
        @commands = element
    link: (scope, element, attrs, breadcrumbCtrl) ->
      breadcrumbCtrl.addGroup scope
      scope.isOpen = scope.isActive
      if scope.isActive then angular.element(element).addClass 'active'

      index = breadcrumbCtrl.getIndexOfGroup(scope) % 7
      scope.cssClass = if index > 0 then "bclevel" + index
      if !scope.isLast and !scope.isActive then angular.element(element).addClass(scope.cssClass)

      scope.$watch 'isOpen', (value) ->
        if value
          angular.element(element).removeClass(scope.cssClass)
          scope.isHover = !scope.isActive and scope.isOpen
          breadcrumbCtrl.closeOthers scope
          breadcrumbCtrl.setCommands scope
        else
          if !scope.isLast and !scope.isActive then angular.element(element).addClass(scope.cssClass)
  ])

  .directive('breadcrumbCommands', () ->
    restrict: 'E'
    replace: true
    templateUrl: 'directives/breadcrumb/breadcrumb-commands.tpl.html'
    scope:
      commands: '='
  )
