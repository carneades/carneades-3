# Copyright (c) 2
# 014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular'
  'root'
  'classes'
  'utils'
  'common/services/i18nNotifications'
  'common/services/httpRequestTracker'
  'common/resources/themes'
  'bucketnav/nav'
], (angular, cn) ->
  "use strict"

  carneades = cn.carneades

  modules = [
    'services.i18nNotifications'
    'services.httpRequestTracker'
    'resources.themes'
    'ui.carneades.bucketnav'
  ]

  module = angular.module 'app.controllers', modules

  module.filter 'theme', () ->
    return (val) ->
      if val? then val else 'default'


  cssDirective = ($state) ->
    restrict: 'E'
    replace: true
    scope:
      theme: '=?'
    template: """
      <link
        rel="stylesheet"
        ng-href="api/projects/{{name | theme}}/theme/css/{{name | theme}}.css"
        media="screen" />
    """
    link: (scope, elm) ->
      scope.$watch 'theme', (newVal) ->
        scope.name = $state.current.data?.theme or newVal

  module.directive 'cssInject', cssDirective


  footerDirective = ($state, $stateParams, ThemeLoader) ->
    restrict: 'E'
    replace: true
    scope:
      theme: '=?'
    template: "<div ng-bind-html='footerTpl'></div>"
    link: (scope, elm) ->
      onLoadingFooterSuccess = (data) ->
        scope.footerTpl = carneades.readData data

      onLoadingFooterError = (data) ->
        scope.footerTpl = ''
        console.log 'theme could not be loaded...'

      scope.$watch 'theme', (newVal, oldVal) ->
        val = $state.current.data?.theme or newVal or 'default'
        theme = new ThemeLoader pid: val, did: 'footer.tpl'
        theme.then onLoadingFooterSuccess, onLoadingFooterError

  module.directive 'footerLoader', footerDirective


  bannerDirective = ($state, $stateParams, ThemeLoader) ->
    restrict: 'E'
    replace: true
    scope:
      theme: '=?'
    template: "<div ng-bind-html='bannerTpl'></div>"
    link: (scope, elm) ->
      onLoadingBannerSuccess = (data) ->
        scope.bannerTpl = carneades.readData data

      onLoadingBannerError = (data) ->
        scope.bannerTpl = ''
        console.log 'theme could not be loaded...'

      scope.$watch 'theme', (newVal, oldVal) ->
        val = $state.current.data?.theme or newVal or 'default'
        theme = new ThemeLoader pid: val, did: 'banner.tpl'
        theme.then onLoadingBannerSuccess, onLoadingBannerError


  module.directive 'bannerLoader', bannerDirective


  headDirective = ($rootScope, $stateParams, $compile, $state) ->
    link = (scope, elem) ->
      html = [
        '<link '
        'rel="stylesheet" '
        'ng-repeat="(routeCtrl, cssUrl) in routeStyles" '
        'ng-href="{{cssUrl}}" '
        '/>'
      ].join ''

      elem.append($compile(html)(scope))
      scope.routeStyles = {}
      $rootScope.$on '$stateChangeStart', (
      event, toState, toParams, fromState, fromParams) ->
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
      link: link
    }


  module.directive 'head', headDirective


  #############################################################################
  ## SubnavController
  #  Updates the page navigation when the event '$stateChangeSuccess' has been
  #  fired. The controller resolves the state names which are provided by the
  #  current state and collects them in a new data structure called commands.
  #############################################################################
  class SubnavController extends carneades.Controller
    @.$inject = [
      '$scope'
      '$state'
    ]

    constructor: (@scope, @state) ->
      @scope.$on '$stateChangeSuccess', =>
        @.update()

      @.update()

    update: ->
      builder = (params...) =>
        create = (label, state, clazz) =>
          return {label: label, state: state, clazz: clazz}

        createCommands = ($state,states...) =>
          commands = []
          for state in states
            label = $state.get(state).label
            commands.push create label, state, undefined
            #commands.push create '', undefined, 'divider'

          # since last item is a divider we must get rid off it
          #if commands.length > 0 then commands.pop()
          return commands

        return ($state) => return createCommands($state, params...)

      _commands = []
      if @state.current.data and @state.current.data.commands
        _commands = builder(@state.current.data.commands...) @state
      @scope.commands = _commands


  module.controller 'SubnavController', SubnavController


  #############################################################################
  ## HeaderController
  #
  #############################################################################
  class HeaderController extends carneades.Controller
    @.$inject = [
      '$scope'
      '$location'
      '$state'
      'breadcrumbService'
      '$cnBucket'
    ]

    constructor: (@scope, @location, @state, @breadcrumbService, @cnBucket) ->
      @scope.$on '$stateChangeSuccess', =>
        #@breadcrumbService.updateStates()
        @cnBucket.append @state
        @.update()

        data = @state.$current.self.data
        _isSubNavDisplayed = data and data.commands and data.commands.length > 0

        # In order to update the list passed to ng-repeat properly
        #_navigatedStates = angular.copy @breadcrumbService.getStates()
        _navigatedStates = angular.copy @cnBucket.getBucketItems()
        _subNavStatesReversed = @._getReversedNavigationStates _navigatedStates

        @scope = carneades.extend @scope,
          navigatedStates: _navigatedStates
          bcTop: @breadcrumbService.peek()
          subNavStatesReversed: _subNavStatesReversed
          navBcCollapsed: true
          navCollapsed: true
          hasHistory: !@cnBucket.isEmpty()
          #hasHistory: !@breadcrumbService.isEmpty()
          isSubNavDisplayed: _isSubNavDisplayed

      @scope = carneades.extend @scope,
        openView: @._openView
        hasHistory: false

      @.update()

    _openView: (name, params) -> @state.go name, params

    _getReversedNavigationStates: (states) ->
      return states.slice(0, states.length-1).reverse()

    update: ->
      builder = (params...) =>
        create = (label, state, clazz) =>
          return {label: label, state: state, clazz: clazz}

        createCommands = ($state,states...) =>
          commands = []
          for state in states
            label = $state.get(state).label
            commands.push create label, state, undefined
            #commands.push create '', undefined, 'divider'

          # since last item is a divider we must get rid off it
          #if commands.length > 0 then commands.pop()
          return commands

        return ($state) => return createCommands($state, params...)

      _commands = []
      if @state.current.data and @state.current.data.commands
        _commands = builder(@state.current.data.commands...) @state
      @scope.commands = _commands


  module.controller 'HeaderCtrl', HeaderController
