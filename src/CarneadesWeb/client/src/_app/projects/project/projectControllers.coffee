# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular'
], (angular) ->
  angular.module('project.controllers', [])

  .controller('ProjectViewCtrl', ($scope, $state, project) ->
    $scope.project = project
    $scope.$stateParams.mid = 1
    $scope.$stateParams.db = 'main'
    $scope.$stateParams.nid = 1
    $scope.$state.$current.self.tooltip = project.title

    $scope.newArgumentGraph = () ->
      $state.transitionTo 'home.projects.project.new', {pid: project.id}

    return @
  )

  .controller('ProjectNewArgGraphCtrl', ($scope, $state, $stateParams, Project) ->
    $scope.ag =
      name: "",
      header:
        description: {en: "", de: "", fr: "", it: "", sp: "", nl: ""},
        title: ""

    $scope.onSave = () ->
      pid = $stateParams.pid
      p = Project.get {}, pid: pid
      params = pid: pid, db: $scope.ag.name
      p.name = $scope.ag.name
      p.header = $scope.header
      p.$saveAg()
      #$state.transitionTo 'home.projects.project.outline', params

      #ag.save($stateParams, $scope.ag).$promise.then(
      #  (v) ->
      #    $state.transitionTo 'home.projects.project.outline', {pid: $stateParams.pid, db: $scope.ag.name}
      #  (e) ->
      #    console.log 'error', e
      #)

    return @
  )
