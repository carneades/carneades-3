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
      $state.transitionTo url, $stateParams
      
    $scope.legalprofiles = legalprofiles
    $scope.new = _new

  .controller 'LegalprofilesNewCtrl', ($scope, $state, $stateParams, $translate, project, theory, legalprofileInfo) ->

    console.log 'theory', theory
    console.log 'ids', legalprofileInfo.getTheoryIds theory
    
    _title = $translate.instant 'projects.legalprofile.new'

    _save = ->
      console.log 'save'
      legalprofile.pid = $stateParams.pid
      legalprofile.lpid = $stateParams.lpid
      legalprofile.save()

    _cancel = ->
      url = 'home.projects.project.legalprofiles'
      $state.transitionTo url, $stateParams

    legalprofile =
      metadata: {}
      default: false
      rules: ({ruleid: id, value: 1.0} for id in legalprofileInfo.getTheoryIds theory)

    _getRuleIndex = (scheme) ->
      for rule, idx in legalprofile.rules
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
    $scope.legalprofile = legalprofile
    
    $scope.valueOptions = [
      {value: 1.0, name: "in"},
      {value: 0.0, name: "out"},
      {value: 0.5, name: "undecided"},
    ]

    return undefined
