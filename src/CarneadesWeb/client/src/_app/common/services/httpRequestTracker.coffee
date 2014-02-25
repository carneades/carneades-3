define ["angular"], (angular) ->
  "use strict"
  angular.module("services.httpRequestTracker", []).factory "httpRequestTracker", ["$http", ($http) ->
    httpRequestTracker = {}
    httpRequestTracker.hasPendingRequests = ->
      $http.pendingRequests.length > 0

    httpRequestTracker
  ]
