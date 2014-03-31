# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', 'angular-resource', './adminControllers'], (angular) ->
  angular.module('admin.states', ['ngResource'])
  .config ['$stateProvider',  ($stateProvider) ->
    states = [
      {
        name: 'admin'
        label: 'Projects administration'
        url: '/admin'
        templateUrl: 'admin.tpl.html'
        controller: 'AdminCtrl',
        # resolve:
        #   entity: ['$resource', ($resource) ->
        #     $resource '../carneades/api/admin/entities/markos?uri=:uri']
        # },
          }
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
  ]
