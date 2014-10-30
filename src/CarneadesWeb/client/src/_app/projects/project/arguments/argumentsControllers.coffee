# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular'
], (angular) ->
  # If you want to merge two arrays without creating a new object

  Array::where = (query) ->
    return [] if typeof query isnt "object"
    hit = Object.keys(query).length
    @filter (item) ->
      match = 0
      for key, val of query
        match += 1 if item[key] is val
      if match is hit then true else false

  _resolveRoleKey = (p, roles) ->
    if isFinite p.role
      p.role = roles[p.role].title

  _resolveRoleKeys = (premises, roles) ->
    for p in premises
      _resolveRoleKey p, roles

  mergeRolesD = (a1, a2) ->
    for o in a2
      contains = a1.where title: o.title
      if contains.length is 0
        a1.push id: a1.length + 1, title: o.title

  mergePremisesD = (a1, a2, roles) ->
    for o in a2
      contains = a1.where role: o.role
      if contains.length is 0
        _resolveRoleKey o, roles
        a1.push o

  extend = (object, properties) ->
    for key, val of properties
      object[key] = val
    object


  angular.module('arguments.controllers', [
    'pascalprecht.translate',
    'angular-capitalize-filter'
  ])

  .controller 'ArgumentNewCtrl', ($scope, $state, $stateParams,
  $translate, Argument, statements, conclusion, breadcrumbService,
  theory, editorService) ->
    _normalize = () ->
      return extend {},
        header:
          title: ''
          description:
            en: ''
            de: ''
            fr: ''
            it: ''
            sp: ''
            nl: ''
        pro: true
        scheme: ''
        strict: false
        weight: 0.5
        conclusion: if conclusion then conclusion.id else ''
        premises: []

    _addPremise = () ->
      editorService.addPremise $scope.argument

    _deletePremise = (p) ->
      editorService.deletePremise $scope.argument, p

    _showModel = () ->
      $scope.tabModel = true
      $scope.tabMetadata = false

    _showMetadata = () ->
      $scope.tabModel = false
      $scope.tabMetadata = true

    _getSchemeTitle = (model) ->
      return editorService.getSchemeTitle model, theory.schemes

    _getStatementText = (model) ->
      return editorService.getStatementText model, statements

    _getStatement = (model) ->
      return editorService.getStatement model, statements


    repoRoles = []
    repoPremises = []

    _getRoles = ({premises}) ->
      roles = []
      unless premises then return roles
      i = 0
      for p in premises
        roles.push id: i, title: p.role
        i = i + 1
      return roles

    _getPremises = ({premises}) ->
      ps = []
      unless premises then return ps
      for p in premises
        if p.statement? and typeof p.statement.text is 'string'
          ps.push p
        else
          p.statement = {}
          p.statement.id = ''
          p.statement.text = ''
          ps.push p

      return ps

    _initRolesPremises = (a) ->
      if a.premises?.length > 0
        repoRoles = _getRoles a
        repoPremises = _getPremises a

    _updatePremises = (newVal, oldVal) ->
      if oldVal is '' and $scope.argument.premises.length > 0
        _initRolesPremises $scope.argument

      roles = repoRoles
      premises = repoPremises
      if newVal?
        newRoles = _getRoles newVal
        newPremises = _getPremises newVal

        if premises.length > 0
          _resolveRoleKeys premises, roles
          mergePremisesD repoPremises, newPremises, roles
        else repoPremises = newPremises

        if roles.length > 0
          mergeRolesD repoRoles, newRoles
        else repoRoles = newRoles

        $scope.premiseRoles = repoRoles
        $scope.argument.premises = []
        $scope.argument.premises.push repoPremises...

    $scope.$watch 'argument.scheme', (newVal, oldVal) ->
      _updatePremises newVal, oldVal

    _onSave = () ->
      pid = $stateParams.pid
      db = $stateParams.db
      _updatePremises $scope.argument

      for p in $scope.argument.premises
        key = 'translation'
        if key of p then delete p[key]

      Argument.save({pid: pid, db: db}, {
        header: $scope.argument.header
        pro: $scope.argument.pro
        scheme: "(#{$scope.argument.scheme.id})"
        weight: $scope.argument.weight
        conclusion: $scope.argument.conclusion
        premises: $scope.argument.premises
      }).$promise.then((a) ->
        url = 'home.projects.project.arguments.argument'
        params = pid: pid, db: db, aid: a.id
        $state.transitionTo url, params
      )

    $scope = extend $scope,
      statements: statements
      argument: _normalize()
      theory: theory
      addPremise: _addPremise
      deletePremise: _deletePremise
      tabModel: true
      tabMetadata: false
      showModel: _showModel
      showMetadata: _showMetadata
      onSave: _onSave
      onCancel: editorService.onCancel
      languages: editorService.getLanguages()
      getSchemeTitle: _getSchemeTitle
      getStatementText: _getStatementText
      editorOptions: editorService.getCodeMirrorOptions()
      title: $translate.instant 'projects.createargument'
      tooltipPremise: $translate.instant 'tooltip.premise'
      tooltipCancel: $translate.instant 'tooltip.cancel'
      tooltipSave: $translate.instant 'tooltip.argument.save'
