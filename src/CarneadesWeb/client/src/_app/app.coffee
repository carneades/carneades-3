# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global define
define [
  'require'
  "angular"
  'root'
  "angular-bootstrap"
  "angular-ui-router"
  "angular-ui-utils"
  "angular-ui-slider"
  "angular-ui-select"
  "projects/projectsModule"
  "lican/licanModule"
  "admin/adminModule"
  "appStates"
  "appControllers"
  "common/directives/breadcrumb/breadcrumb"
  "common/providers/css-injector"
  "common/directives/page-navigation/page-navigation"
  "templates/app"
  "angular-translate"
  "angular-translate-loader-static-files"
  "common/directives/svg-include"
  "common/directives/markdown/markdown"
  "common/directives/share/share"
  'common/directives/editor/editor'
  'common/resources/themes'
  'showdown'
  'hallo'
  'to-markdown'
  'jquery'
  'angular-capitalize-filter'
  'codemirror'
  'codemirror-clj'
  'codemirror-addon/edit/matchbrackets'
  'codemirror-addon/edit/closebrackets'
  'angular-ui-codemirror'
  'angular-selectize'
  'utils'
], (require, angular, cn) ->
  window.CodeMirror = require 'codemirror'

  configure = (
    $urlRouterProvider,
    $cssProvider,
    $stateProvider,
    $provide,
    $translateProvider,
    $uiViewScrollProvider,
    markdownConverterProvider,
    $locationProvider
  ) ->
    carneades = (converter) ->
      return [
        #  ? title ? syntax
        type: "lang"
        regex: "\\[@([^\\,]+)[^\\]]*\\]"
        replace: (match, citation_key) ->
          return [
            "<a "
            "href='/carneades/#/projects/%PID%/%DB%/outline"
            "?scrollTo=#{citation_key}'>"
            "#{match}"
            "</a>"
          ].join ''
      ,
        type: "output"
        filter: (source) ->
          source.replace /file:\/\/(\w+)\/(\w+)/g, (match, project, document) ->
            return "carneadesws/documents/#{project}/#{document}"
      ]

    if typeof window.Showdown isnt
    'undefined' and
    window.Showdown and
    window.Showdown.extensions
      window.Showdown.extensions.carneades = carneades

    markdownConverterProvider.config(extensions: ['carneades'])

    $translateProvider.useStaticFilesLoader(
      prefix: '/carneades/languages/',
      suffix: '.json'
    )

    $translateProvider.preferredLanguage 'en'
    # $translateProvider.useLocalStorage()

    $urlRouterProvider.otherwise "/"
    $urlRouterProvider.when '', '/projects'
    # disable autoscrolling on ui-views
    $uiViewScrollProvider.useAnchorScroll()


  init = ($rootScope, $state, $stateParams, ThemeLoader) ->
    $rootScope.$state = $state
    $rootScope.$stateParams = $stateParams

    $rootScope.$on '$stateChangeStart', (e, to) ->
      $rootScope.viewLoading = true

    $rootScope.$on '$stateChangeSuccess', (e, to) ->
      $rootScope.viewLoading = false


  modules = [
    "ui.bootstrap"
    'ui.utils'
    'ui.select'
    'ui.codemirror'
    "ui.carneades.breadcrumb"
    'ui.carneades.share'
    'ui.carneades.editor'
    "ui.slider"
    "directives.pagenav"
    "directives.svg.include"
    "ui.router"
    "css.injector"
    "app.states"
    "app.controllers"
    "templates.app"
    "projects.module"
    "lican.module"
    "admin.module"
    "pascalprecht.translate"
    'markdown'
    'angular-capitalize-filter'
    'resources.themes'
    'selectize'
  ]

  module = angular.module 'app', modules


  module.config [
    '$urlRouterProvider'
    '$cssProvider'
    '$stateProvider'
    '$provide'
    '$translateProvider'
    '$uiViewScrollProvider'
    'markdownConverterProvider'
    '$locationProvider'
    configure
  ]


  module.run [
    '$rootScope'
    '$state'
    '$stateParams'
    'ThemeLoader'
    init
  ]
