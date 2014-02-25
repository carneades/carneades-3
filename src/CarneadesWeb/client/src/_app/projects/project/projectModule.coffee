define ['angular', './projectStates', './outline/outlineModule', './map/mapModule' ,'./statement/statementModule', './argument/argumentModule', './theory/theoryModule'], (angular) ->
  "use strict"
  angular.module 'project.module', ['project.states', 'outline.module', 'map.module', 'statement.module', 'argument.module', 'theory.module']
