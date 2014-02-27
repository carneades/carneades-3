# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
