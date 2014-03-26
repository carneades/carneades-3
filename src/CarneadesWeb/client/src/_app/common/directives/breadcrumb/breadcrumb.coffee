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

    render = (states, nameOfCurrentState) ->
      index = 0
      bIsActiveFound = false
      angular.forEach states, (s) ->
        index++
        bIsActive = (nameOfCurrentState == s.name)
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
        if (index == 0 and length == 0) then return 'BC_INIT'
        if (index == 0 and length > 0) then return 'BC_APPEND_ITEM'
        if (index > 0 and index < length) then return 'BC_ITEM_CLICKED'
        if (index > 0 and index == length) then return 'BC_LAST_ITEM_CLICKED'
        return 'EVENT_NOT_REGISTERED'

      idxEntryClicked = getIndexOfItemClicked()
      iSizeOfBreadcrumb = _navStates.length
      setIndexOfItemClicked 0
      switch getEvent idxEntryClicked, iSizeOfBreadcrumb
        when 'BC_INIT'
          angular.forEach state.path, (s) ->
            navigationStates.push buildState s.self, stateParams
        when 'BC_LAST_ITEM_CLICKED'
          navigationStates = _navStates
          navigationStates.push buildState state.self, stateParams
        when 'BC_APPEND_ITEM'
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

      getNavigationStates: ($state, $stateParams) ->
        return render getNavigationStates($state.$current, $stateParams), $state.$current.name
    ]
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

      $scope.changeState = (name, cIndex) ->
        if name
          index = getIndexByName $scope.states, name
          $breadcrumb.setIndexOfItemClicked index + 1
          $state.go $scope.states[index].commands[cIndex].state, $stateParams
          _index = index + 1

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
    controller: ($scope, $element) ->
      $scope.openCommandsView = () ->
        $scope.bcOpen()
        $scope.isHover = true
    link: (scope, element, attrs) ->
      index = scope.index % 7
      scope.cssClass = if index > 0 then "bclevel" + index
      if scope.state.isActive then angular.element(element).addClass "active"
      else if scope.state.isLast then angular.element(element).addClass "last"
      else angular.element(element).addClass scope.cssClass
  )
  .directive('breadcrumbCommands', () ->
    restrict: 'E'
    replace: true
    transclude: true
    templateUrl: 'directives/breadcrumb/breadcrumb-commands.tpl.html'
  )
  .directive('breadcrumbCommand', () ->
    restrict: 'E'
    replace: true
    scope:
      command: '='
      index: '='
      bcChange: '&'
    templateUrl: 'directives/breadcrumb/breadcrumb-command.tpl.html'
    controller: ($scope, $element, $attrs, $breadcrumb) ->
      $scope.setState = () ->
        $scope.bcChange()
  )