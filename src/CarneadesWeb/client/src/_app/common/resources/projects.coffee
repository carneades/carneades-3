#global define
define ["angular", "angular-resource"], (angular) ->
  "use strict"
  services = angular.module("resources.projects", ["ngResource"])
  services.factory "Project", ["$resource", ($resource) ->
    $resource "../api/projects/:pid",
      pid: "@pid"

  ]
  services.factory "MultiProjectLoader", ["Project", "$q", (Project, $q) ->
    ->
      delay = $q.defer()
      Project.query ((project) ->
        delay.resolve project
      ), ->
        delay.reject "Unable to fetch projects"

      delay.promise
  ]
  services.factory "ProjectLoader", ["Project", "$q", (Project, $q) ->
    (params) ->
      delay = $q.defer()
      Project.get params, ((project) ->
        delay.resolve project
      ), ->
        delay.reject "Unable to fetch project " + params.id

      delay.promise
  ]
  services
