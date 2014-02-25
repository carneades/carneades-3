define ['angular', './project/projectModule', './projectsStates', 'templates/projects'], (angular) ->
  "use strict"
  angular.module 'projects.module', ['projects.states', 'project.module', 'templates.projects']
