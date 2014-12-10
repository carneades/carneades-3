# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular'], (angular) ->
  angular.module('services.projectInfo', []).service 'projectInfo',
  () ->
    # theory can be written with an absolute path like default/walton_schemes
    # or with a relative path like copyright_policies.

    getTheoryProject: (project) ->
      theory = project.theory
      res = theory.split '/'
      if res.length is 1 then project.id else res[0]

    getTheoryName: (project) ->
      res = project.theory.split '/'
      if res.length is 1 then res[0] else res[1]
