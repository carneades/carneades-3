define ['angular', './argumentStates', './argumentControllers'], (angular) ->
  "use strict"
  angular.module 'argument.module', ['argument.controllers', 'argument.states']
