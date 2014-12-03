# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular'
], (angular) ->
  return angular.module('outline.controllers', [])

  .controller 'OutlineRootCtrl', ($scope, $state, $stateParams, $previousState,
    $translate, $location, $window, ag, tproject, scroll, tpid, markos) ->

    $stateParams.tpid = tpid
    $scope = angular.extend $scope,
      project: ag
      scrollTo: scroll.scrollTo
      tooltipEdit: $translate.instant 'tooltip.outline.edit'
      tooltipNewStatement: $translate.instant 'tooltip.statement.new'
      tooltipNewArgument: $translate.instant 'tooltip.argument.new'
      tooltipShare: $translate.instant 'tooltip.share'
      currentUrl: $location.absUrl()
      isSharing: false
      onShare: ->
        $scope.isSharing = !$scope.isSharing
      shareOnMarkos: ->
        markos.share($scope.currentUrl)
      shareOnTwitter: ->
        console.log 'share on twitter'
        _url = escape($scope.currentUrl)
        _text = escape(ag.title)
        link = "https://twitter.com/intent/tweet?url=#{_url}&text=#{_text}"
        $window.open link
        return true
      openMetadataEditor: ->
        $state.go 'home.projects.project.edit', $stateParams
        $previousState.memo 'newMetadataEditor'
      openArgumentEditor: ->
        $state.go 'home.projects.project.arguments.new', $stateParams
        $previousState.memo 'newArgumentEditor'
      openStatementEditor: ->
        $state.go 'home.projects.project.statements.new', $stateParams
        $previousState.memo 'newStatementEditor'

    return @

  .controller 'OutlineIssuesCtrl', ($scope, issues) ->
    $scope = angular.extend $scope,
      issues: issues

    return @

  .controller 'OutlineReferencesCtrl', ($scope, references) ->
    $scope = angular.extend $scope,
      references: references
      hasReferences: not (references.length == 0)

    return @

  .controller 'OutlineOutlineCtrl', ($scope, outline) ->
    $scope = angular.extend $scope,
      outline: outline

    console.log outline

    return @
