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
    _navigationStates = []
    position = 0


    setStorageService = (service) ->
      storageService = service

    getVisitedStates = ->
      visited = storageService.get 'visited'
      return visited ?= []

    storeVisitedStates = (states) -> storageService.set 'visited', states

    setIndexOfItemClicked = (value) ->
      position = value

    getIndexOfItemClicked = () ->
      return position

    ##########################################################
    render = (states, scope) ->
      index = 0
      bIsActiveFound = false
      angular.forEach states, (s) ->
        index++
        bIsActive = (scope.$state.$current.name == s.name)
        s.isActive = bIsActive
        s.isLast = (index == states.length)
        s.isDepreciated = bIsActiveFound

        if bIsActive then bIsActiveFound = true

    ##########################################################
    cmdsToNavigation = (commands = [], stateParams) ->
      results = []
      for c in commands
        results.push {label: c.label, state: c.state, params: stateParams}
      return results

    buildState = (state, stateParams) ->
      label: state.label
      name: state.name
      params: stateParams
      commands: cmdsToNavigation state.commands, stateParams

    buildNavigationStates = (state, stateParams, _navStates) ->
      navigationStates = []
      getEvent = (index, length) ->
        if (index == -1) then return 'BC_APPEND_ITEM'
        if (index == 0) then return 'BC_ITEM_NOT_CLICKED'
        if (index == length) then return 'BC_LAST_ITEM_CLICKED'
        if (index > 0) then return 'BC_ITEM_CLICKED'
        return 'EVENT_NOT_REGISTERED'

      idxEntryClicked = getIndexOfItemClicked()
      iSizeOfBreadcrumb = _navStates.length

      switch getEvent idxEntryClicked, iSizeOfBreadcrumb
        when 'BC_ITEM_NOT_CLICKED'
          angular.forEach state.path, (s) ->
            navigationStates.push buildState s.self, stateParams
        when 'BC_LAST_ITEM_CLICKED' or 'BC_APPEND_ITEM'
          navigationStates = _navStates
          navigationStates.push buildState state.self, stateParams
        when 'BC_ITEM_CLICKED'
          temp = _navStates
          navigationStates = temp.slice 0, idxEntryClicked
          depreciated = temp.slice idxEntryClicked + 1, iSizeOfBreadcrumb
          #bAppendDepreciated = currentState.self.name == $breadcrumb.getNavigationStates()[entryPositionClicked].name
          navigationStates.push buildState state.self, stateParams
          angular.forEach depreciated, (s) ->
            navigationStates.push s

      return navigationStates

    ##########################################################
    ##########################################################

    getNavigationStates = (state, stateParams) ->
      _navigationStates = buildNavigationStates state, stateParams, _navigationStates
      return _navigationStates

    $get: ['localStorageService', (localStorageService) ->
      getVisitedStates: ->
        setStorageService localStorageService
        return getVisitedStates()

      storeVisitedStates: (states) ->
        storeVisitedStates states

      getIndexOfItemClicked: () ->
        return getIndexOfItemClicked()

      setIndexOfItemClicked: (value) ->
        setIndexOfItemClicked value

      getNavigationStates: (scope) ->
        return render getNavigationStates(scope.$state.$current, scope.$stateParams), scope
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
        $breadcrumb.setIndexOfItemClicked $scope.index + 1

        # $navigationStates = $breadcrumb.getNavigationStates()
        # if $scope.index + 1 == $navigationStates.length
        #   $breadcrumb.push state, $stateParams
        # else
        #   $breadcrumb.replace $scope.index + 1, state, $stateParams

        $state.go $state.get(state), params
  )
