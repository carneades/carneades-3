# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-translate',
  '../../../common/resources/legalprofiles',
  '../../../common/resources/theory',
  '../../../common/services/projectInfo',
  '../../../common/services/legalprofileInfo'
], (angular) ->
  angular.module('legalprofiles.states', [
    'resources.legalprofiles',
    'legalprofiles.controllers',
    'resources.theories',
    'services.projectInfo',
    'services.legalprofileInfo'
  ])
  
  .config ($stateProvider) ->
    states = [
      name: "home.projects.project.legalprofiles"
      label: 'Legal profiles'
      url: "/legalprofiles"
      views:
        'content@':
          templateUrl: 'projects/project/legalprofiles/legalprofiles.jade'
          controller: 'LegalprofilesCtrl'
      resolve:
        legalprofiles: (MultiLegalprofilesLoader, $stateParams) ->
              return new MultiLegalprofilesLoader $stateParams
    ,
      name: "home.projects.project.legalprofiles.new"
      url: "/new?copyFrom"
      views:
        'content@':
          templateUrl: 'projects/project/legalprofiles/legalprofile/edit.jade'
          controller: 'LegalprofilesNewCtrl'
      resolve:
        project: (ProjectLoader, $stateParams) ->
          new ProjectLoader($stateParams)
        scroll: 'scroll'
        legalprofileInfo: 'legalprofileInfo'
        tproject: (ProjectLoader, $stateParams) ->
          return new ProjectLoader $stateParams
        theory: (TheoryLoader, projectInfo, tproject, $stateParams) ->
          tpid = projectInfo.getSchemesProject tproject
          tid = projectInfo.getSchemesName tproject
          $stateParams.tpid = tpid
          $stateParams.tid = tid
          return new TheoryLoader $stateParams
        copiedLegalProfile: ($stateParams, LegalprofileLoader) ->
          console.log "copyFrom=", $stateParams.copyFrom
          if $stateParams.copyFrom?
            return new LegalprofileLoader {pid: $stateParams.pid, lpid: $stateParams.copyFrom}
          else
            undefined
      ]
      
    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined
