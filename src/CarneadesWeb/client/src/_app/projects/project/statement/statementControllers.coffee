define ['angular'], (angular) ->
  getType = (obj) ->
    switch Object::toString.call(obj)
      when '[object String]'
        'String'
      when '[object Object]'
        'Object'
      when '[object Array]'
        'Array'
      else
        'Literal'

  angular.module('statement.controllers', [])
    .controller('StatementCtrl', ['$scope', '$stateParams', 'statement', ($scope, $stateParams, statement) ->
      $scope.statement = statement
      $scope.pid = $stateParams.pid
      $scope.db = $stateParams.db

      $scope.getType = (obj) ->
        getType(obj);

      $scope.$watch 'statement', (statement) ->
        if (statement)
          $scope.table = [
            { key: 'id', value: statement.id }
            { key: 'atom', value: statement.atom ?= '-' }
            { key: 'main_issues', value: statement.main_issues ?= '-' }
            { key: 'standard', value: statement.standard ?= '-' }
            { key: 'value', value: statement.value ?= '-' }
            { key: 'weight', value: statement.weight ?= '-' }
          ]

          entries = {}
          entries.text = statement.text if statement.text?
          entries.header = statement.header if statement.header?
          entries.pro = statement.pro if statement.pro?
          entries.con = statement.con if statement.con?.length > 0
          $scope.entries = entries

        undefined
      , true

      undefined
    ])
