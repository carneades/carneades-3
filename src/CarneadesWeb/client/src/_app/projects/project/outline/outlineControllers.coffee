# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular'
], (angular) ->
  return angular.module('outline.controllers', [])

  .controller 'OutlineRootCtrl', ($scope, $state, $stateParams,
    $translate, $location, $window, ag, tproject, scroll, tpid) ->
      
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
        console.log 'share on markos'
      shareOnTwitter: ->
        console.log 'share on twitter'
        _url = escape($scope.currentUrl)
        _text = escape(ag.title)
        $window.open("https://twitter.com/intent/tweet?url=#{_url}&text=#{_text}");
        return true
        
    return @

  .controller 'OutlineIssuesCtrl', ($scope, issues) ->
    $scope = angular.extend $scope,
      issues: issues

    return @

  .controller 'OutlineReferencesCtrl', ($scope, references) ->
    emptyReferences = (references) ->
      (v for k,v of references when v? and k != '$promise' and k != '$resolved')
      .length is 0

    $scope = angular.extend $scope,
      references: references
      hasReferences: not emptyReferences references

    return @

  .controller 'OutlineOutlineCtrl', ($scope, outline) ->
    $scope = angular.extend $scope,
      outline: outline

    return @
