# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ["angular"], (angular) ->
  "use strict"
  angular.module("providers.breadcrumb", ["ui.router.state"]).provider "$breadcrumb", ->
    options = {}
    pushNonexistentState = (array, state) ->
      isStateKnown = false
      angular.forEach array, (value) ->
        isStateKnown = true  if not isStateKnown and angular.equals(value, state)

      array.push state  unless isStateKnown
      isStateKnown

    {
      setPrefixState: (prefixStateName) ->
        options.prefixStateName = prefixStateName
        undefined

      $get: ["$state", ($state) ->
        getStatesChain: ->
          chain = []
          prefixState = $state.get(options.prefixStateName)
        
          # Prefix state
          if options.prefixStateName
            if prefixState
              pushNonexistentState chain, prefixState
            else
              throw "Bad configuration: prefixState \"" + options.prefixStateName + "\" unknown"
          angular.forEach $state.$current.path, (value) ->
            pushNonexistentState chain, value.self
            undefined

          chain
      ]
    }
