# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-translate',
  '../../../common/resources/references',
  './referencesControllers'
], (angular) ->
  angular.module('references.states', [
    'resources.metadata.references',
    'references.controllers'
  ])
  
  .config ($stateProvider) ->
    states = [
      name: "home.projects.project.references"
      label: 'References'
      url: "/:db/references"
      views:
        'content@':
          templateUrl: 'projects/project/references/references.jade'
          controller: 'ReferencesCtrl'
      resolve:
        references: (MultiReferenceLoader, $stateParams) ->
              return new MultiReferenceLoader $stateParams
    ]
    
    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
