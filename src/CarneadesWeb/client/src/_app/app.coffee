#global define
define ["angular", "angular-bootstrap", "angular-ui-router", "projects/projectsModule", "lican/licanModule", "appStates", "appControllers", "angular-markdown", "common/misc/carneades", "common/directives/breadcrumb/breadcrumb", "templates/app", "templates/common"], (angular) ->
  "use strict"
  angular.module("app", ["ui.bootstrap", "ui.bootsrap.breadcrumb", "ui.router", "app.states", "app.controllers", "templates.app", "templates.common", "projects.module", "lican.module", "angular-markdown"])
  .run(['$rootScope', '$state', '$stateParams', ($rootScope, $state, $stateParams) ->
    $rootScope.$state = $state
    $rootScope.$stateParams = $stateParams
  ])
  .config(["$urlRouterProvider", "$stateProvider", "$httpProvider", "$provide", ($urlRouterProvider, $stateProvider, $httpProvider, $provide) ->
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
  ])
  .factory('requestNotificationChannel', ['$rootScope', ($rootScope) ->
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
  ])

  .directive('loading', ['requestNotificationChannel', (requestNotificationChannel) ->
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
  ])

  .constant("I18N.MESSAGES",
    "errors.route.changeError": "Route change error"
    "crud.user.save.error": "Something went wrong when saving a user..."
    "crud.project.save.success": "A project with id '{{id}}' was saved successfully."
    "crud.project.remove.success": "A project with id '{{id}}' was removed successfully."
    "crud.project.save.error": "Something went wrong when saving a project..."
    "login.reason.notAuthorized": "You do not have the necessary access permissions.  Do you want to login as someone else?"
    "login.reason.notAuthenticated": "You must be logged in to access this part of the application."
    "login.error.invalidCredentials": "Login failed.  Please check your credentials and try again."
    "login.error.serverError": "There was a problem with authenticating: {{exception}}.")
    .constant('_START_REQUEST_', '_START_REQUEST_')
    .constant('_END_REQUEST_', '_END_REQUEST_')
