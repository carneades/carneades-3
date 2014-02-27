# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ["angular"], (angular) ->
  "use strict"
  angular.module("services.notifications", []).factory "notifications", ["$rootScope", ($rootScope) ->
    notifications =
      STICKY: []
      ROUTE_CURRENT: []
      ROUTE_NEXT: []

    notificationsService = {}
    addNotification = (notificationsArray, notificationObj) ->
      throw new Error("Only object can be added to the notification service")  unless angular.isObject(notificationObj)
      notificationsArray.push notificationObj
      notificationObj

    $rootScope.$on "$routeChangeSuccess", ->
      notifications.ROUTE_CURRENT.length = 0
      notifications.ROUTE_CURRENT = angular.copy(notifications.ROUTE_NEXT)
      notifications.ROUTE_NEXT.length = 0
      undefined

    notificationsService.getCurrent = ->
      [].concat notifications.STICKY, notifications.ROUTE_CURRENT

    notificationsService.pushSticky = (notification) ->
      addNotification notifications.STICKY, notification

    notificationsService.pushForCurrentRoute = (notification) ->
      addNotification notifications.ROUTE_CURRENT, notification

    notificationsService.pushForNextRoute = (notification) ->
      addNotification notifications.ROUTE_NEXT, notification

    notificationsService.remove = (notification) ->
      angular.forEach notifications, (notificationsByType) ->
        idx = notificationsByType.indexOf(notification)
        notificationsByType.splice idx, 1  if idx > -1


    notificationsService.removeAll = ->
      angular.forEach notifications, (notificationsByType) ->
        notificationsByType.length = 0
        undefined
      undefined


    notificationsService
  ]
