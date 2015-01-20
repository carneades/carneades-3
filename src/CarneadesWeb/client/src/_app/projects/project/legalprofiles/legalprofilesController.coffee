# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular',
  'angular-translate'
], (angular) ->

  angular.module('legalprofiles.controllers', [])
  .controller 'LegalprofilesCtrl', ($scope, $state, $stateParams, legalprofiles) ->
    _new = ->
      url = 'home.projects.project.legalprofiles.new'
      $state.go url, $stateParams
      
    $scope.legalprofiles = legalprofiles
    $scope.new = _new

  .controller 'LegalprofilesNewCtrl', ($scope, $state, $stateParams, $translate, project, theory, legalprofileInfo, Legalprofile, copiedLegalProfile) ->

    _createLegalProfileTemplate = (copy) ->
      if copy?
        copy.metadata.title = copy.metadata.title + " " + ($translate.instant 'projects.legalprofile.copy')
        delete copy.id
        copy
      else
        metadata:
          title: $translate.instant 'projects.legalprofile.new'
          description:
            en: ""
            de: ""
            fr: ""
            it: ""
            sp: ""
            nl: ""
        default: false
        rules: ({ruleid: id, value: 1.0} for id in legalprofileInfo.getTheoryIds theory)
      
    _title = $translate.instant 'projects.legalprofile.new'

    _legalprofile = _createLegalProfileTemplate(copiedLegalProfile)
    
    _save = ->
      Legalprofile.save($stateParams, _legalprofile).$promise.then((l) ->
        url = 'home.projects.project.legalprofiles.legalprofile'
        params = pid: $stateParams.pid, db: $stateParams.db, lpid: l.id
        $state.go url, params, reload: true)

    _cancel = ->
      url = 'home.projects.project.legalprofiles'
      $state.go url, $stateParams
    
    _getRuleIndex = (scheme) ->
      for rule, idx in _legalprofile.rules
        if rule.ruleid == scheme.id
          return idx

    $scope.isSchemeOut = legalprofileInfo.isSchemeOut
    $scope.isSchemeIn = legalprofileInfo.isSchemeIn
    $scope.isSchemeUndecided = legalprofileInfo.isSchemeUndecided
    $scope.getRuleIndex = _getRuleIndex
    $scope.save = _save
    $scope.cancel = _cancel
    $scope.title = _title
    $scope.section = theory
    $scope.project = project
    $scope.legalprofile = _legalprofile
    
    $scope.valueOptions = [
      {value: 1.0, name: "in"},
      {value: 0.0, name: "out"},
      {value: 0.5, name: "undecided"},
    ]

    console.log "copiedFrom=", copiedLegalProfile
    
    return undefined
