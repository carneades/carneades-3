define ['angular'], (angular) ->
  angular.module('state.util', [])
  .provider('$stateUtil', () ->
    builder = () ->
      resolve = {}
      add: (name, func) ->
        resolve[name] = func
        return @
      build: () ->
        return resolve

    cmdBuilder = (params...) ->
      create = (label, state, clazz) ->
        return {label: label, state: state, clazz: clazz}
      command = ($state,state) ->
        return create $state.get(state).label, state, undefined
      divider = () ->
        return create '', undefined, 'divider'

      createCommands = ($state,states...) ->
        commands = []
        for state in states
          commands.push command($state, state)
          commands.push divider()

        # since last item is a divider we must get rid off it
        if commands.length > 0 then commands.pop()
        return commands

      return ($state) ->
        return createCommands($state, params...)

    $get: () ->
      builder: () ->
        return builder()
      cmdBuilder: (params...) ->
        return cmdBuilder(params...)
  )
