# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular',
  './legalprofileControllers',
  '../../../../common/resources/legalprofiles',
  '../../../../common/resources/theory',
  '../../../../common/services/projectInfo',
  '../../../../common/services/legalprofileInfo'
], (angular) ->
  angular.module('legalprofile.states', [
    'legalprofile.controllers',
    'resources.legalprofiles',
    'resources.theories',
    'services.projectInfo',
    'services.legalprofileInfo'    
  ])

  .config ($stateProvider) ->
    states = [
      name: 'home.projects.project.legalprofiles.legalprofile'
      label: 'Legal profile'
      url: '/:lpid?scrollTo'
      data:
        commands: ['home.projects.project.legalprofiles']
      views:
        'content@':
          templateUrl: 'projects/project/legalprofiles/legalprofile/view.jade'
          controller: 'LegalprofileViewCtrl'
          resolve:
            project: (ProjectLoader, $stateParams) ->
              new ProjectLoader($stateParams)
            scroll: 'scroll'
            legalprofileInfo: 'legalprofileInfo'
            tproject: (ProjectLoader, $stateParams) ->
              return new ProjectLoader $stateParams
            legalprofile: (LegalprofileLoader, $stateParams) ->
              return new LegalprofileLoader $stateParams
            theory: (TheoryLoader, projectInfo, tproject, $stateParams) ->
              tpid = projectInfo.getTheoryProject tproject
              tid = projectInfo.getTheoryName tproject
              $stateParams.tpid = tpid
              $stateParams.tid = tid
              return new TheoryLoader $stateParams
      ,
        name: 'home.projects.project.legalprofiles.legalprofile.edit'
        label: 'Edit legal profile'
        url: '/edit'
        data:
          commands: ['home.projects.project.legalprofiles']
        views:
          "subnav@":
            template: '<page-navigation-sm-offset-2 ng-show="commands.length > 0"><page-navigation-item cmd="c" ng-repeat="c in commands"></page-navigation-item></page-navigation-sm-offset-2>'
            controller: 'SubnavController'
          'content@':
            templateUrl: 'projects/project/legalprofiles/legalprofile/edit.jade'
            controller: 'LegalprofileEditCtrl'
            resolve:
              project: (ProjectLoader, $stateParams) ->
                new ProjectLoader($stateParams)
              scroll: 'scroll'
              tproject: (ProjectLoader, $stateParams) ->
                return new ProjectLoader $stateParams
              legalprofile: (LegalprofileLoader, $stateParams) ->
                return new LegalprofileLoader $stateParams
              theory: (TheoryLoader, projectInfo, tproject, $stateParams) ->
                tpid = projectInfo.getTheoryProject tproject
                tid = projectInfo.getTheoryName tproject
                $stateParams.tpid = tpid
                $stateParams.tid = tid
                return new TheoryLoader $stateParams
          
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
