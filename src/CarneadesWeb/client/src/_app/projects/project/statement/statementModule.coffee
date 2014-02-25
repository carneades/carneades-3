define ['angular', './statementStates', './statementControllers'], (angular) ->
  "use strict"
  angular.module 'statement.module', ['statement.controllers', 'statement.states']
