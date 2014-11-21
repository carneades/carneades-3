# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular'
], (angular) ->
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
      p.role = roles[p.role]?.title

  _resolveRoleKeys = (premises, roles) ->
    for p in premises
      _resolveRoleKey p, roles

  mergeRolesD = (a1, a2) ->
    for o in a2
      contains = a1.where title: o.title
      if contains.length is 0
        a1.push id: a1.length + 1, title: o.title

  mergePremisesD = (a1, a2, roles, newRoles) ->
    merge = []
    for p in a1
      containsRole = newRoles.where title: p.role
      if (p.statement.id.length isnt 0) or
      (containsRole.length is 1)
        merge.push p

    for o in a2
      contains = a1.where role: o.role
      if contains.length is 0
        _resolveRoleKey o, roles
        merge.push o

    return merge


  extend = (object, properties) ->
    for key, val of properties
      object[key] = val
    object

  angular.module('argument.controllers', [
    'pascalprecht.translate',
    'angular-capitalize-filter'
  ])

  .controller 'ModalArgumentRemoveCtrl', ($scope, $modalInstance) ->
    $scope.ok = () ->
      $modalInstance.close true

    $scope.cancel = () ->
      $modalInstance.dismiss 'cancel'

    return @

  .controller 'ArgumentViewCtrl', ($scope, $state, $stateParams, $translate,
    $modal, $previousState, argument, Argument, project, editorService) ->

    _remove = () ->
      modalInstance = $modal.open(
        templateUrl: 'projects/project/arguments/argument/modalArgumentRemove.jade'
        controller: 'ModalArgumentRemoveCtrl'
      )

      modalInstance.result.then((m) ->
        Argument.delete($stateParams, argument).$promise.then((data) ->
          url = 'home.projects.project.outline'
          $state.transitionTo url, $stateParams, reload: true
        )
      )

    _showModel = () ->
      $scope.tabModel = true
      $scope.tabMetadata = false

    _showMetadata = () ->
      $scope.tabModel = false
      $scope.tabMetadata = true

    _getValueText = ({value}) ->
      key = 'projects.argument.value.unclear'
      if value <= 0.25
        key = 'projects.argument.value.unacceptable'
      else if value >= 0.75
        key = 'projects.argument.value.acceptable'

      return $translate.instant key

    _getConclusionText = ({strict, pro}) ->
      _getStrict = (value) ->
        strict_pro = 'projects.strict_pro_conclusion'
        strict_con = 'projects.strict_con_conclusion'
        return if value then strict_pro else strict_con

      _getNonStrict = (value) ->
        nonstrict_pro = 'projects.nonstrict_pro_conclusion'
        nonstrict_con = 'projects.nonstrict_con_conclusion'
        return if value then nonstrict_pro else nonstrict_con

      key = if strict then _getStrict pro else _getNonStrict pro
      return $translate.instant key

    _getTooltip = ({scheme, id}) ->
      return if scheme then scheme.header.title else id

    _edit = () ->
      url = 'home.projects.project.arguments.argument.edit'
      $state.go url, $stateParams
      $previousState.memo 'newArgumentEditor'

    _openStatement = (sid) ->
      url = 'home.projects.project.statements.statement'
      params = pid: $stateParams.pid, db: $stateParams.db, sid: sid
      $state.go url, params
      $previousState.memo 'newStatementEditor'

    argument.valueText = _getValueText argument
    $scope = extend $scope,
      argument: argument
      conclusion_text: _getConclusionText argument
      project: project
      edit: _edit
      remove: _remove
      openStatement: _openStatement
      tabModel: true
      tabMetadata: false
      showModel: _showModel
      showMetadata: _showMetadata
      isMapInitialized: editorService.isMapInitialized
      tooltipEdit: $translate.instant 'tooltip.argument.edit'
      tooltipRemove: $translate.instant 'tooltip.argument.remove'

    $state.$current.self.tooltip = _getTooltip argument

    return @

  .controller 'ArgumentEditCtrl', ($scope, $state, $stateParams, $translate,
    statements, argument, Argument, theory, breadcrumbService,
    editorService, $cnBucket, $previousState) ->
    _showModel = () ->
      $scope.tabModel = true
      $scope.tabMetadata = false

    _showMetadata = () ->
      $scope.tabModel = false
      $scope.tabMetadata = true

    _onSave = () ->
      _updatePremises $scope.argument
      for p in $scope.argument.premises
        key = 'translation'
        if key of p then delete p[key]

      if typeof $scope.argument.scheme is 'object'
        argument.scheme = $scope.argument.scheme.id
        argument.scheme = "(#{argument.scheme})"
      argument.conclusion = $scope.argument.conclusion.id
      Argument.update($stateParams, argument).$promise.then((data) ->
        $cnBucket.remove $state.$current
        state = $previousState.get 'newArgumentEditor'
        $state.go state.state.name, state.params, reload: true
        $previousState.forget 'newArgumentEditor')

    _addPremise = () ->
      editorService.addPremise $scope.argument

    _deletePremise = (index) ->
      repoPremises.splice index, 1
      editorService.deletePremise $scope.argument, index

    _getScheme = (model) ->
      return editorService.getScheme model, theory.schemes

    _getStatementText = (model) ->
      return editorService.getStatementText model, statements

    _getStatement = (model) ->
      return editorService.getStatement model, statements

    _getSchemeId = ({scheme}) ->
      return scheme.slice 1, -1

    _getConclusionId = ({conclusion}) ->
      return conclusion.slice 1, -1

    if argument.conclusion?
      argument.conclusion = argument.conclusion.id

    if typeof argument.scheme is 'string'
      id = _getSchemeId argument
      argument.scheme = _getScheme id

    if typeof argument.conclusion is 'string'
      argument.conclusion = _getStatement argument.conclusion

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
      else
        repoRoles = []
        repoPremises = []

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
          repoPremises = mergePremisesD(
            repoPremises, newPremises, roles, newRoles
          )
        else repoPremises = newPremises

        if roles.length > 0
          mergeRolesD repoRoles, newRoles
        else repoRoles = newRoles

        $scope.premiseRoles = repoRoles
        $scope.argument.premises = []
        $scope.argument.premises.push repoPremises...

    $scope.$watch 'argument.scheme', (newVal, oldVal) ->
      _updatePremises newVal, oldVal

    _initRolesPremises argument

    $scope = extend $scope,
      statements: statements
      title: $translate.instant 'projects.editargument'
      argument: argument
      theory: theory
      languages: editorService.getLanguages()
      getStatementText: _getStatementText
      editorOptions: editorService.getCodeMirrorOptions()
      addPremise: _addPremise
      deletePremise: _deletePremise
      onSave: _onSave
      onCancel: -> editorService.onCancel 'newArgumentEditor'
      tabModel: true
      tabMetadata: false
      showModel: _showModel
      showMetadata: _showMetadata
      tooltipPremise: $translate.instant 'tooltip.premise'
      tooltipCancel: $translate.instant 'tooltip.cancel'
      tooltipSave: $translate.instant 'tooltip.argument.save'

    return @
