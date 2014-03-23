# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
    .controller('ArgumentCtrl', ($scope, argument) ->
      $scope.argument = argument
      $scope.pid = $scope.$stateParams.pid
      $scope.db = $scope.$stateParams.db

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
    )
    .controller('ArgumentEditCtrl', ($scope, $filter, argument) ->
      $scope.pid = $scope.$stateParams.pid
      $scope.db = $scope.$stateParams.db
      $scope.getType = (obj) ->
        getType(obj)

      $scope.jsonData =
        Name: 'Joe'
        Address: {
          Street: 'Neverland 42'
          Foo: 'Bar'
        }

      undefined
    )
