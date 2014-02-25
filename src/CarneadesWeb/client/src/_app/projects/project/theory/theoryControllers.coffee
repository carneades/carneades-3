define ['angular'], (angular) ->
  angular.module('theory.controllers', [])
  .controller('TheoryCtrl', ['$scope', '$stateParams', '$location', '$anchorScroll', 'theory',
  ($scope, $stateParams, $location, $anchorScroll, theory) ->
    $scope.showMetadatum = (k, v) ->
      v? and (k not in ['id', 'description', 'title'])

    $scope.gotoSection = (section) ->
      setTimeout ->
        window.scrollTo(window.pageXOffset, window.pageYOffset - 90)
        
        old = $location.hash()
        $location.hash section
        $anchorScroll()
        $location.hash old

      , 200
      
    

    $scope.stateParams = $stateParams
    $scope.lang = theory.lang
    $scope.section = theory

    if $stateParams.scrollTo?
      $scope.gotoSection $stateParams.scrollTo
    
  ])
