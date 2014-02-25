define ["angular"], (angular) ->
  "use strict"
  angular.module("services.localizedMessages", []).factory "localizedMessages", ["$interpolate", "I18N.MESSAGES", ($interpolate, i18nmessages) ->
    handleNotFound = (msg, msgKey) ->
      msg or "?" + msgKey + "?"

    get: (msgKey, interpolateParams) ->
      msg = i18nmessages[msgKey]
      returnValue = undefined
      if msg
        returnValue = $interpolate(msg)(interpolateParams)
      else
        returnValue = handleNotFound(msg, msgKey)
      returnValue
  ]
