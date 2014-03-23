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
    navigationStates = []
    position = 0

    setStorageService = (service) ->
      storageService = service

    getVisitedStates = ->
      visited = storageService.get 'visited'
      return visited ?= []

    storeVisitedStates = (states) -> storageService.set 'visited', states

    getStateChain = (state) ->
      chain = []
      angular.forEach state.path, (s) ->
        chain.push s.self
      return chain

    getPositionInVisited = (state, states) ->
      for x, i in states
        return i if x.name == state.name
      return -1

    push = (state) ->
      navigationStates.push state

    replace = (index, state, params) ->
      navigationStates[index].state = state
      navigationStates[index].params = params

    setEntryPositionClicked = (value) ->
      position = value

    getEntryPositionClicked = () ->
      return position

    clear = () ->
      navigationStates = []

    getNavigationStates = () ->
      return navigationStates

    $get: ['localStorageService', (localStorageService) ->
      getVisitedStates: ->
        setStorageService localStorageService
        return getVisitedStates()

      storeVisitedStates: (states) ->
        storeVisitedStates states

      getStateChain: (state) ->
        return getStateChain state

      getPositionInVisited: (state, states) ->
        return getPositionInVisited state, states

      push: (state) ->
        push state

      replace: (index, state, params) ->
        replace index, state, params

      clear: () ->
        clear()

      getEntryPositionClicked: () ->
        return getEntryPositionClicked()

      setEntryPositionClicked: (value) ->
        setEntryPositionClicked value

      getNavigationStates: () ->
        return getNavigationStates()
    ]
  )

  .controller('BreadcrumbController', ($scope, $compile, $attrs, breadcrumbConfig) ->
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

    @setIndex = (groupScope) ->
      $scope.index = groupScope.index

    @setCommands = (groupScope) ->
      $scope.commands = groupScope.commands

    @getIndexOfGroup = (group) ->
      @groups.indexOf group

    @removeGroup = (group) ->
      index = @groups.indexOf group
      if index != -1 then @groups.splice @groups.indexOf(group), 1

      undefined

    undefined
  )

  .directive('breadcrumb', () ->
    restrict: 'EA'
    controller: 'BreadcrumbController'
    transclude: true
    replace: true
    templateUrl: 'directives/breadcrumb/breadcrumb.tpl.html'
  )

  .directive('breadcrumbGroup', ($parse) ->
    require: '^breadcrumb'
    restrict: 'EA'
    transclude: true
    replace: true
    templateUrl: 'directives/breadcrumb/breadcrumb-entry.tpl.html'
    scope:
      label: '@'
      commands: '='
      index: '='
      isActive: '='
      isLast: '='
    controller: () ->
      @setLabel = (element) ->
        @label = element
      @setCommands = (element) ->
        @commands = element
      @setIndex = (element) ->
        @index = element
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
          breadcrumbCtrl.setIndex scope
        else
          if !scope.isLast and !scope.isActive then angular.element(element).addClass(scope.cssClass)
  )

  .directive('breadcrumbCommands', ($breadcrumb) ->
    restrict: 'E'
    replace: true
    templateUrl: 'directives/breadcrumb/breadcrumb-commands.tpl.html'
    scope:
      commands: '='
      index: '='
    controller: ($scope, $breadcrumb, $state, $stateParams) ->
      $scope.append = (state, params) ->
        # $breadcrumb.storeVisitedStates $breadcrumb.getStateChain($state.get state)
        # state = $state.get state
        $breadcrumb.setEntryPositionClicked $scope.index + 1

        # $navigationStates = $breadcrumb.getNavigationStates()
        # if $scope.index + 1 == $navigationStates.length
        #   $breadcrumb.push state, $stateParams
        # else
        #   $breadcrumb.replace $scope.index + 1, state, $stateParams

        $state.go $state.get(state), params
  )
