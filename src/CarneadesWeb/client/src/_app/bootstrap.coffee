require ['angular', 'requirejs-domready', 'app', 'appStates'], (angular, domReady) ->
  "use strict"
  domReady ->
    angular.bootstrap document, ['app']
