# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
