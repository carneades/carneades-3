# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', './project/projectModule', './projectsStates', 'templates/projects'], (angular) ->
  "use strict"
  angular.module 'projects.module', ['projects.states', 'project.module', 'templates.projects']