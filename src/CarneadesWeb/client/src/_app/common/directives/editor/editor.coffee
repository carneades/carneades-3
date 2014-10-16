# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular',
  'angular-translate',
  '../../services/projectInfo'
], (angular) ->
  extend = (object, properties) ->
    for key, val of properties
      object[key] = val
    object

  angular.module('ui.carneades.editor', [
    'pascalprecht.translate',
    'services.projectInfo',
    'ui.carneades.share'
  ])

  .factory 'editorService', ($state,
  $stateParams, $translate, breadcrumbService, markdownConverter) ->
    # add a new premise to argument.premise
    @addPremise = ({premises}) ->
      premises.push {
        role: ""
        implicit: false
        positive: true
        statement:
          id: ''
      }

    # remove a premise p from argument.premises
    @deletePremise = (argument, p) ->
      argument.premises = (q for q in argument.premises when p.role != q.role)

    @onCancel = () ->
      breadcrumbService.peek()
      breadcrumbService.pop()
      top = breadcrumbService.peek()
      if top?
        breadcrumbService.pop()
        $state.transitionTo top.name, top.params
      else
        $state.transitionTo 'home.projects', $stateParams

    @getLanguages = () ->
      return [
        ['en', 'En'],
        ['de', 'De'],
        ['fr', 'Fr'],
        ['it', 'It'],
        ['sp', 'Sp'],
        ['nl', 'Nl']]

    @fillWithPrefixSuffixes = (arr, prefix, arrSuffixes) ->
      for suffix in arrSuffixes
        arr.push
          name: ($translate.instant [prefix, suffix].join '')
          value: suffix
      return arr

    @getCodeMirrorOptions = () ->
      return {
        lineWrapping : true
        lineNumbers: true
        tabSize: 2
        smartIndent: 2
        theme: 'xq-light'
        matchBrackets: true
        autoCloseBrackets: true
        mode: 'clojure'
      }

    @getSchemeTitle = (model, schemes) ->
      for scheme in schemes
        if model is scheme.id
          return scheme.header.title

    @getStatementText = (model, statements) ->
      for statement in statements
        if model is statement.id
          return statement.text

    @isMapInitialized = (obj) ->
      return (v for k,v of obj when v).length > 0

    @htmlize = (content) ->
      return markdownConverter.makeHtml content

    @markdownize = (content) ->
      html = content.split('\n').map($.trim).filter((line) ->
        return line isnt ''
      ).join '\n'
      return toMarkdown html


    return @

  .directive 'premiseEditor', ->
    restrict: "E"
    templateUrl: "common/directives/editor/premise-editor.jade"
    scope:
      model: '=',
      statements: '=',
      onDelete: '&'
    controller: ($scope) ->
      _setIsPositive = (value) ->
        isPositiveLabel = ['positive', 'negative']
        iSelector = if value then 0 else 1
        $scope.positive = isPositiveLabel[iSelector]

      _setIsImplicit = (value) ->
        isImplicitLabel = ['implicit', 'explicit']
        iSelector = if value then 0 else 1
        $scope.implicit = isImplicitLabel[iSelector]

      $scope.$watch 'model.positive', (value) ->
        _setIsPositive value

      $scope.$watch 'model.implicit', (value) ->
        _setIsImplicit value

      _setIsImplicit $scope.model.positive
      _setIsImplicit $scope.model.implicit

      $scope.formatStatement = (model) ->
        for statement in $scope.statements
          if model is statement.id
            return statement.text

      return @

    link: (scope, elem, attrs) ->
      scope.onDeletePremise = ->
        console.log 'onDeletePremise'
        scope.onDelete()

  .controller 'EditorController', () ->
    @getLanguages = () ->
      return [
        ['en', 'En'],
        ['de', 'De'],
        ['fr', 'Fr'],
        ['it', 'It'],
        ['sp', 'Sp'],
        ['nl', 'Nl']]

    return @

  .directive 'formGroup', ($translate) ->
    restrict: 'E'
    replace: true
    transclude: true
    scope:
      labelKey: '@'
    templateUrl: 'common/directives/editor/form-group.jade'
    link: (scope, element, attrs) ->
      scope.key = $translate.instant scope.labelKey

  .directive 'formGroupSource', ($translate) ->
    restrict: 'E'
    replace: true
    transclude: true
    scope:
      labelKey: '@'
    templateUrl: 'common/directives/editor/form-group-source.jade'
    link: (scope, element, attrs) ->
      scope.key = $translate.instant scope.labelKey

  .directive 'formGroupNoLabel', ($translate) ->
    restrict: 'E'
    replace: true
    transclude: true
    templateUrl: 'common/directives/editor/form-group-no-label.jade'

  .filter 'doublePrecision', () ->
    return (value) ->
      if value?
        return value.toFixed(2).toString()
      else
        return '0.00'

  .filter 'selectedById', () ->
    return (items, id) ->
      return (items.filter (s) -> s.id is id)

  .filter 'lastKey', () ->
    return (items, key) ->
      filtered = []
      last = []
      angular.forEach items, (item) ->
        if item.hasOwnProperty key
          last.push item

      return items

  .directive 'radioButtons', () ->
    restrict: "E"
    templateUrl: "common/directives/editor/radio-buttons.jade"
    scope:
      model: '='
    link: (scope, element, attrs) ->
      scope.activate = (val) ->
        scope.model = val

  .directive 'properties', () ->
    restrict: "E"
    templateUrl: "common/directives/editor/properties.jade"
    scope:
      keys: "=",
      model: "="
    controller: ($scope, $translate, projectInfo) ->
      @getSchemesProject = (project) ->
        schemes = project.schemes
        res = schemes.split '/'
        if res.length is 1 then project.id else res[0]

      @getSchemesName = (project) ->
        res = project.schemes.split '/'
        if res.length is 1 then res[0] else res[1]

      @getTypeOfDisplay = (k, v) ->
        if k is 'scheme' and v.formalized
          'formalizedScheme'
        else if k is 'scheme' and not v.formalized
          'unformalizedScheme'
        else if k is 'standard'
          'standard'
        else
          'default'

      @getStandardName = (s) -> return $translate.instant "projects.#{s}"

      @schemesProject = ({project}) ->
        return projectInfo.getSchemesProject project

      @schemesName = ({project}) ->
        return projectInfo.getSchemesName project

      return @

    link: (scope, element, attrs, propertiesCtrl) ->
      scope = extend scope,
        project: scope.$parent.project
        pid: scope.$parent.pid
        db: scope.$parent.db
        standardName: propertiesCtrl.getStandardName
        schemesProject: propertiesCtrl.schemesProject scope.$parent
        schemesName: propertiesCtrl.schemesName scope.$parent
        typeOfDisplay: propertiesCtrl.getTypeOfDisplay
        getSchemesName: propertiesCtrl.getSchemesName
        getSchemesProject: propertiesCtrl.getSchemesProject

  .directive 'hallo', ($parse, $timeout, editorService) ->
    restrict: 'AC'
    scope: true
    require: '?ngModel'
    compile: (tElement, tAttrs) ->
      return (scope, elm, attr, ngModel) ->
        unless ngModel? then return

        params = scope.$eval attr.hallo
        bound = angular.isDefined attr.ngModel

        $timeout(() ->
          contents = if bound then scope.$eval(attr.ngModel) else elm.html()
          contents = editorService.htmlize(contents or '')
          elm.hallo(params).html(contents).addClass 'editable'
        , 100)

        if angular.isDefined(attr.ngModel)?
          model = $parse attr.ngModel
          elm.bind 'halloactivated', () ->
            scope.$apply(() ->
              elm.html(editorService.htmlize elm.html())
          )

          elm.bind 'hallodeactivated', () ->
            scope.$apply(() ->
              data = editorService.markdownize elm.html()
              model.assign scope, data
          )

  .directive 'halloEditor', () ->
    restrict: 'E'
    templateUrl: "common/directives/editor/hallo-editor.jade"
    require:'ngModel'
    scope:
      ngModel: '='
      lang: '='
      name: '='

  .directive 'multilangHalloEditor', () ->
    restrict: 'E'
    templateUrl: "common/directives/editor/multilang-hallo-editor.jade"
    require:'ngModel'
    scope:
      ngModel: '='
      languages: '='
      name: '@'
    link: (scope) ->

  .directive 'metadataEditTab', () ->
    restrict: 'E'
    templateUrl: "common/directives/editor/edit/metadata-edit-tab.jade"
    scope:
      ngModel: '='
      activeOn: '='
