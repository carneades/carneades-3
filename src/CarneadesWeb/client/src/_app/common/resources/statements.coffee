define ["angular", "angular-resource"], (angular) ->
  "use strict"
  services = angular.module("resources.statements", ["ngResource"])
  services.factory "Statement", ["$resource", ($resource) ->
    $resource "../api/projects/:pid/:db/statements/:sid",
      pid: "@pid"
      db: "@db"
      sid: "@sid"

  ]
  services.factory "MultiStatementLoader", ["Statement", "$q", (Statement, $q) ->
    ->
      delay = $q.defer()
      Statement.query ((statement) ->
        delay.resolve statement
      ), ->
        delay.reject "Unable to fetch nodes"

      delay.promise
  ]
  services.factory "StatementLoader", ["Statement", "$q", (Statement, $q) ->
    (params) ->
      delay = $q.defer()
      Statement.get params, ((statement) ->
        delay.resolve statement
      ), ->
        delay.reject "Unable to fetch argument!"

      delay.promise
  ]
  services
