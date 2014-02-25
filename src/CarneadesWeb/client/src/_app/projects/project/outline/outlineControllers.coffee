define ['angular'], (angular) ->
  "use strict"
  angular.module('outline.controllers', [])
  .controller('OutlineCtrl', ['$scope', '$stateParams', '$location', '$anchorScroll', 'project', 'node', 'metadata', ($scope, $stateParams, $location, $anchorScroll, project, node, metadata) ->
    $scope.args = node
    $scope.project = project
    $scope.meta = metadata
    $scope.pid = $stateParams.pid
    $scope.db = $stateParams.db

    $scope.gotoSection = (section) ->
      $location.hash section
      $anchorScroll()
      setTimeout ->
        window.scrollTo(window.pageXOffset, window.pageYOffset - 90)
      , 200
    ])
