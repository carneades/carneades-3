#global define
define ["angular", "angular-resource"], (angular) ->
  "use strict"
  services = angular.module("resources.metadata", ["ngResource"])
  services.factory "Metadata", ["$resource", ($resource) ->
    $resource "../api/projects/:pid/:db/metadata/:mid",
      pid: "@pid"
      db: "@db"
      mid: "@mid"

  ]
  services.factory "MultiMetadataLoader", ["Metadata", "$q", (Metadata, $q) ->
    ->
      delay = $q.defer()
      Metadata.query ((metadata) ->
        delay.resolve metadata
      ), ->
        delay.reject "Unable to fetch arguments"

      delay.promise
  ]
  services.factory "MetadataLoader", ["Metadata", "$q", (Metadata, $q) ->
    (params) ->
      delay = $q.defer()
      Metadata.get params, ((metadata) ->
        delay.resolve metadata
      ), ->
        delay.reject "Unable to fetch metadata"

      delay.promise
  ]
  services