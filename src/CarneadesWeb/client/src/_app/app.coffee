# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global define
define ["angular", "angular-bootstrap", "angular-ui-router",
"projects/projectsModule", "lican/licanModule", "admin/adminModule",
"appStates", "appControllers",
"angular-markdown", "common/directives/breadcrumb/breadcrumb",
"common/providers/css-injector",
"common/directives/page-navigation/page-navigation", "templates/app",
"templates/common", "angular-translate",
"angular-translate-loader-static-files"], (angular) ->
  angular.module("app", ["ui.bootstrap", "ui.bootsrap.breadcrumb",
  "directives.pagenav", "ui.router", "css.injector",
  "app.states", "app.controllers", "templates.app", "templates.common",
  "projects.module", "lican.module", "admin.module", "angular-markdown",
  "pascalprecht.translate"])

  .run(($rootScope, $state, $stateParams) ->
    $rootScope.$state = $state
    $rootScope.$stateParams = $stateParams
  )

  .config(($urlRouterProvider, $stateProvider, $httpProvider, $provide, $translateProvider) ->

    $translateProvider.useStaticFilesLoader(
      prefix: '/carneades/languages/',
      suffix: '.json'
    )

    $translateProvider.preferredLanguage 'en'
    # $translateProvider.useLocalStorage()

    $urlRouterProvider.otherwise "/"

    $provide.factory "requestInterceptor", ($q, $injector) ->
      requestEnded = ->
        # get $http via $injector because of circular dependency problem
        $http = $http or $injector.get '$http'
        # don't send notification until all requests are complete
        if $http.pendingRequests.length < 1
          # get requestNotificationChannel via $injector because of circular dependency problem
          notificationChannel = notificationChannel or $injector.get 'requestNotificationChannel'
          # send a notification requests are complete
          notificationChannel.requestEnded()
          undefined

      request: (config) ->
        notificationChannel = notificationChannel or $injector.get 'requestNotificationChannel'
        notificationChannel.requestStarted()
        config

      requestError: (rejection) ->
        requestEnded()
        $q.reject(rejection)

      response: (response) ->
        requestEnded()
        response

      responseError: (rejection) ->
        requestEnded()
        $q.reject(rejection)

    $httpProvider.interceptors.push 'requestInterceptor'
    undefined
  )
  .factory('requestNotificationChannel', ($rootScope) ->
    # private notification messages
    _START_REQUEST_ = '_START_REQUEST_'
    _END_REQUEST_ = '_END_REQUEST_'

    # publish start request notification
    requestStarted = ->
      $rootScope.$broadcast _START_REQUEST_
      undefined

    # publish end request notification
    requestEnded = ->
      $rootScope.$broadcast _END_REQUEST_
      undefined

    # subscribe to start request notification
    onRequestStarted = ($scope, handler) ->
      $scope.$on _START_REQUEST_, (event) ->
        handler()
        undefined
      undefined

    # subscribe to end request notification
    onRequestEnded = ($scope, handler) ->
      $scope.$on _END_REQUEST_, (event) ->
        handler()
        undefined
      undefined

    requestStarted:  requestStarted
    requestEnded: requestEnded
    onRequestStarted: onRequestStarted
    onRequestEnded: onRequestEnded
  )

  .directive('loading', (requestNotificationChannel) ->
    restrict: "A"
    link: (scope, element, $modal) ->
      # hide the element initally
      element.hide()
      startRequestHandler = ->
        # got the request start notification, show the element4
        element.show()
        undefined

      endRequestHandler = ->
        # got the request start notification, show the element
        element.hide()
        undefined

      requestNotificationChannel.onRequestStarted scope, startRequestHandler
      requestNotificationChannel.onRequestEnded scope, endRequestHandler

      undefined
  )
  .constant('_START_REQUEST_', '_START_REQUEST_')
  .constant('_END_REQUEST_', '_END_REQUEST_')
