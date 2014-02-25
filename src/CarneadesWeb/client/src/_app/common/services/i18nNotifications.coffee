define ["angular", "./localizedMessages", "./notifications"], (angular) ->
  "use strict"
  angular.module("services.i18nNotifications", ["services.notifications", "services.localizedMessages"]).factory "i18nNotifications", ["localizedMessages", "notifications", (localizedMessages, notifications) ->
    prepareNotification = (msgKey, type, interpolateParams, otherProperties) ->
      angular.extend
        message: localizedMessages.get(msgKey, interpolateParams)
        type: type
      , otherProperties

    I18nNotifications =
      pushSticky: (msgKey, type, interpolateParams, otherProperties) ->
        notifications.pushSticky prepareNotification(msgKey, type, interpolateParams, otherProperties)

      pushForCurrentRoute: (msgKey, type, interpolateParams, otherProperties) ->
        notifications.pushForCurrentRoute prepareNotification(msgKey, type, interpolateParams, otherProperties)

      pushForNextRoute: (msgKey, type, interpolateParams, otherProperties) ->
        notifications.pushForNextRoute prepareNotification(msgKey, type, interpolateParams, otherProperties)

      getCurrent: ->
        notifications.getCurrent()

      remove: (notification) ->
        notifications.remove notification

    I18nNotifications
  ]
