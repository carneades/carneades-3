define ['angular'], (angular) ->
  angular.module('css.injector', [])
  .provider('$cssInjector', () ->
    singlePageMode = false
    head = angular.element document.querySelector('head')
    scope = {}

    _initScope = () ->
      if scope is undefined then scope = head.scope()

    addStylesheet = (href, $compile) ->
      _initScope()
      if scope.injectedStylesheets is undefined
        scope.injectedStylesheets = []
        head.append $compile('<link data-ng-repeat=\"stylesheet in injectedStylesheets\" data-ng-href=\"{{stylesheet.href}}\" rel=\"stylesheet\" />')(scope)
          # else
          #   for(var i in scope.injectedStylesheets)
          #     if scope.injectedStylesheets[i].href is href
          #       return;

      scope.injectedStylesheets.push href: href

    removeAll = () ->
      _initScope()
      if scope.injectedStylesheets isnt undefined
        scope.injectedStylesheets = []

    $get: ($compile, $rootScope) ->
      $rootScope.$on '$locationChangeStart', () ->
        if singlePageMode is true then removeAll()

      add: (href) ->
        addStylesheet href, $compile
      removeAll: () ->
        removeAll()
  )
