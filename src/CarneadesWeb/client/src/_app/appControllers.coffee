define ['angular', 'common/services/i18nNotifications', 'common/services/httpRequestTracker'], (angular) ->
  "use strict"
  angular.module('app.controllers', ['services.i18nNotifications', 'services.httpRequestTracker']).controller('AppCtrl', ['$scope', '$location', 'i18nNotifications', ($scope, $location, i18nNotifications) ->
    $scope.notifications = i18nNotifications
    $scope.removeNotification = (notification) ->
      i18nNotifications.remove(notification)
      undefined

    $scope.$on '$routeChangeError', (event, current, previous, rejection) ->
       i18nNotifications.pushForCurrentRoute 'errors.route.changeError', 'error', {}, {rejection: rejection}
       undefined

    undefined
    ]
  )
  .controller 'HeaderCtrl', ['$breadcrumb', '$scope', '$location', 'notifications', 'httpRequestTracker', ($breadcrumb, $scope, $location, notification, httpRequestTracker) ->
    $scope.hasPendingRequests = ->
      httpRequestTracker.hasPendingRequests()

    $scope.oneAtATime = true;

    $scope.test = {state: 'home', label: 'Home'}
    $scope.$watch (-> $scope.$state.current), ((value) ->

      convertCommands = (commands = []) ->
        results = []
        for c in commands
          results.push {label: c.label, state: $scope.$state.href(c.state, $scope.$stateParams)}
        return results

      convertStates = (states) ->
        results = []
        for state in states
          results.push {label: state.label, state: state.name, commands: convertCommands(state.commands)}

        return results

      getStatePosition = (state, states) ->
        for x, i in states
          return i if x.name == state.name
        return -1

      position = -1
      if value?.name
        value.$stateParams = $scope.$stateParams
        visited = $breadcrumb.getVisitedStates()
        position = getStatePosition(value, visited)
        if position < 0
          visited.push value
          position = visited.length - 1
        else
          visited[position] = value

        $breadcrumb.storeVisitedStates visited

        $scope.visitedStates = convertStates visited
        $scope.visitedStates[position].isActive = true
        $scope.visitedStates[$scope.visitedStates.length - 1].isLast = true
    ), true

    undefined
  ]
