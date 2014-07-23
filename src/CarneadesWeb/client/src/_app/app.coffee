# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global define
define [
  "angular",
  "angular-bootstrap",
  "angular-ui-router",
  "angular-ui-utils",
  "projects/projectsModule",
  "lican/licanModule",
  "admin/adminModule",
  "appStates",
  "appControllers",
  "common/directives/breadcrumb/breadcrumb",
  "common/providers/css-injector",
  "common/directives/page-navigation/page-navigation",
  "templates/app",
  "angular-translate",
  "angular-translate-loader-static-files",
  "common/directives/markdown/markdown",
  "common/directives/loader/loader",
  'showdown'
], (angular) ->
  angular.module("app", [
    "ui.bootstrap",
    'ui.utils'
    "ui.bootsrap.breadcrumb",
    "directives.pagenav",
    "directives.loaders",
    "ui.router",
    "css.injector",
    "app.states",
    "app.controllers",
    "templates.app",
    "projects.module",
    "lican.module",
    "admin.module",
    "pascalprecht.translate",
    'markdown',
  ])

  .run(($rootScope, $state, $stateParams) ->
    $rootScope.$state = $state
    $rootScope.$stateParams = $stateParams
  )

  .config((
    $urlRouterProvider,
    $stateProvider,
    $provide,
    $translateProvider,
    $uiViewScrollProvider,
    markdownConverterProvider
  ) ->
    carneades = (converter) ->
      return [
        #  ? title ? syntax
        type: "lang"
        regex: "\\[@([^\\,]+)[^\\]]*\\]"
        replace: (match, citation_key) ->
          "<a href='/carneades/#/projects/pid/db/outline?scrollTo=#{citation_key}'>#{match}</a>";
      ,
        type: "output"
        filter: (source) ->
          source.replace /file:\/\/(\w+)\/(\w+)/g, (match, project, document) ->
            "carneadesws/documents/#{project}/#{document}"
      ]

    if typeof window.Showdown isnt
    'undefined' and
    window.Showdown and
    window.Showdown.extensions
      window.Showdown.extensions.carneades = carneades

    markdownConverterProvider.config(
      extensions: ['carneades']
    )

    $translateProvider.useStaticFilesLoader(
      prefix: '/carneades/languages/',
      suffix: '.json'
    )

    $translateProvider.preferredLanguage 'en'
    # $translateProvider.useLocalStorage()

    $urlRouterProvider.otherwise "/"

    # disable autoscrolling on ui-views
    $uiViewScrollProvider.useAnchorScroll()
  )
