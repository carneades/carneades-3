define ["angular", "angular-ui-router"], (angular) ->
  angular.module("directives.pagenav", ["ui.router.state"])
  .directive('pageNavigation', () ->
    restrict: 'EA'
    replace: true
    transclude: true
    templateUrl: 'directives/page-navigation/page-navigation.tpl.html'
  )
  .directive('pageNavigationItem', () ->
    restrict: 'EA'
    replace: true
    templateUrl: 'directives/page-navigation/page-navigation-item.tpl.html'
    scope:
      cmd: '='
    controller: ($scope, $element, $attrs, $state, $stateParams) ->
      $scope.navOpen = (name) ->
        $state.go name, $stateParams
  )
