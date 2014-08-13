# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular',
  '../../../common/resources/projects',
  '../../../common/resources/metadata',
  '../../../common/resources/references',
  '../../../common/resources/outline',
  '../../../common/resources/issues'
  '../../../common/services/scroll',
  './outlineControllers'
], (angular) ->
  angular.module('outline.states', [
    'resources.projects',
    'resources.metadata',
    'resources.metadata.references',
    'resources.outline',
    'resources.outline.issues',
    'services.scroll',
    'outline.controllers'
  ])

  .config(($stateProvider) ->
    states = [
      {
        name: 'home.projects.project.outline'
        label: 'Outline'
        url: '/:db/outline?scrollTo'
        data:
          commands: ['home.projects.project.map', 'home.projects.project.theory']
        views:
          "content@":
            templateUrl: 'projects/project/outline/outline-main.jade'
            controller: 'OutlineRootCtrl'
            resolve:
              tproject: (ProjectLoader, $stateParams) ->
                return new ProjectLoader $stateParams
              project: (MetadataLoader, $stateParams) ->
                $stateParams.mid = 1
                return new MetadataLoader $stateParams
              scroll: 'scroll'

          "issues@home.projects.project.outline":
            templateUrl: 'projects/project/outline/issues.jade'
            controller: 'OutlineIssuesCtrl'
            resolve:
              issues: ($stateParams, MultiIssueLoader) ->
                return new MultiIssueLoader($stateParams)

          "references@home.projects.project.outline":
            templateUrl: 'projects/project/outline/references.jade'
            controller: 'OutlineReferencesCtrl'
            resolve:
              references: ($stateParams, MultiReferenceLoader) ->
                $stateParams.mid = undefined
                return new MultiReferenceLoader $stateParams

          "outline@home.projects.project.outline":
            templateUrl: 'projects/project/outline/outline.jade'
            controller: 'OutlineOutlineCtrl'
            resolve:
              outline: ($stateParams, MultiOutlineLoader) ->
                return new MultiOutlineLoader $stateParams
      }
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
  )
