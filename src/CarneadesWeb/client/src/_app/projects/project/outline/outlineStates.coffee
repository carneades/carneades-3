# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  '../../../common/resources/metadata',
  '../../../common/resources/references',
  '../../../common/resources/outline',
  '../../../common/resources/issues'
  '../../../common/services/scroll'], (angular) ->

  angular.module('outline.states', [
    'resources.metadata',
    'resources.metadata.references', 'resources.outline',
    'resources.outline.issues', 'services.scroll']
  ).config(($stateProvider) ->
    states = [
      {
        name: 'home.projects.project.outline'
        label: 'Outline'
        url: '/:db/outline?scrollTo'
        commands: [
          label: "Map"
          state: "home.projects.project.map"
        ,
          label: "Theory"
          state: "home.projects.project.theory"
        ]
        views:
          "nav@":
            template: "<bc-navigation></bc-navigation>"
          "content@":
            templateUrl: 'project/outline/outline-main.tpl.html'
            controller: ($scope, $stateParams, scroll, project) ->
              $scope.project = project
              $scope.project.title = project.title
              $scope.scrollTo = scroll.scrollTo

              if $stateParams.scrollTo?
                scroll.scrollTo $stateParams.scrollTo

            resolve:
              project: ($stateParams, MetadataLoader) ->
                $stateParams.mid = 1
                new MetadataLoader($stateParams)
              scroll: 'scroll'

          "issues@home.projects.project.outline":
            templateUrl: 'project/outline/issues.tpl.html'
            controller: ($scope, issues) ->
              $scope.issues = issues
            resolve:
              issues: ($stateParams, MultiIssueLoader) ->
                return new MultiIssueLoader($stateParams)

          "references@home.projects.project.outline":
            templateUrl: 'project/outline/references.tpl.html'
            controller: ($scope, references) ->
              $scope.references = references
            resolve:
              references: ($stateParams, MultiReferenceLoader) ->
                $stateParams.mid = undefined
                return new MultiReferenceLoader($stateParams)

          "outline@home.projects.project.outline":
            templateUrl: 'project/outline/outline.tpl.html'
            controller: ($scope, outline) ->
              $scope.outline = outline
            resolve:
              outline: ($stateParams, MultiOutlineLoader) ->
                return new MultiOutlineLoader($stateParams)
      }
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
  )
