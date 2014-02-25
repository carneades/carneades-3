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

  angular.module('argument.controllers', [])
    .controller('ArgumentCtrl', ['$scope', '$stateParams', 'argument', ($scope, $stateParams, argument) ->
      $scope.argument = argument
      $scope.pid = $stateParams.pid
      $scope.db = $stateParams.db

      $scope.getType = (obj) ->
        getType(obj)

      $scope.$watch 'argument', (argument) ->
        if (argument)
          $scope.table = [
            { key: 'id', value: argument.id }
            { key: 'atom', value: argument.atom ?= '-' }
            { key: 'standard', value: argument.standard ?= '-' }
            { key: 'value', value: argument.value ?= '-' }
            { key: 'weight', value: argument.value ?= '-' }
          ]

          entries = {}
          entries.conclusion = argument.conclusion if argument.conclusion?
          entries.premises = argument.premises if argument.premises?
          $scope.entries = entries

        undefined
      , true
    ])
    .controller('ArgumentEditCtrl', ['$scope', '$filter', '$stateParams', 'argument', ($scope, $filter, $stateParams, argument) ->
      $scope.pid = $stateParams.pid
      $scope.db = $stateParams.db
      $scope.getType = (obj) ->
        getType(obj)

      $scope.jsonData =
        Name: 'Joe'
        Address: {
          Street: 'Neverland 42'
          Foo: 'Bar'
        }

      undefined
    ])
