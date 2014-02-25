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
