# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  './referenceControllers',
  '../../../../common/resources/metadata'
], (angular) ->
  angular.module('reference.states', [
    'reference.controllers'
  ])

  .config ($stateProvider) ->
    states = [
      name: 'home.projects.project.references.reference'
      label: 'Reference'
      url: '/:mid'
      views:
        'content@':
          templateUrl: 'projects/project/references/reference/edit.jade'
          controller: 'ReferenceEditCtrl'
          resolve:
            reference: (MetadataLoader, $stateParams) ->
              return new MetadataLoader $stateParams
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
