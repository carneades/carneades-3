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

      getNavigationStates: (scope) ->
        return render getNavigationStates(scope.$state.$current, scope.$stateParams), scope
    ]
  )

  .directive('breadcrumb', () ->
    restrict: 'EA'
    scope:
      states: '='
    replace: true
    templateUrl: 'directives/breadcrumb/breadcrumb.tpl.html'
    controller: ($scope, $state, $breadcrumb, $compile) ->
      updateVisibleState = () ->
        if $scope.states.length > 0
          $scope.commands = $scope.states[$scope.states.length - 1].commands

      $scope.commands = []

      $scope.setOpen = (index) ->
        $scope.commands = $scope.states[index].commands

      getPositionOfStateByName = (name) ->
        index = 0
        angular.forEach $scope.states, (s) ->
          if s.name = name then return index
          index++
        return 0

      $scope.append = (state, params) ->
        $breadcrumb.setIndexOfItemClicked getPositionOfStateByName(state.name) + 1
        $state.go $state.get(state), params

      $scope.$on '$stateChangeSuccess', () ->
        updateVisibleState()
  )
