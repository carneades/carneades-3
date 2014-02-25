define ["angular", "./mapControllers", "./mapStates"], (angular) ->
  "use strict"
  angular.module 'map.module', ['map.controllers', 'map.states']
