define [
  'angular'
  'root'
], (angular, cn) ->
  'use strict'
  carneades = cn.carneades

  modules = []

  module = angular.module 'ui.carneades.bucketnav', modules


  bucketProvider = ->
    service = {}
    ordering = []
    buckets = {}

    _contains = (state) -> return state.name of buckets

    service.isEmpty = -> return ordering.length is 0

    service.size = -> return ordering.length

    service.add = (state) ->
      if not _contains(state)
        ordering.push state.name

      carneades.del buckets, state.name
      buckets[state.name] = state

    service.remove = (state) ->
      if _contains(state)
        carneades.del buckets, state.name
        index = (ordering.indexOf(state.name))
        if index isnt -1
          ordering.splice index,1

    service.asArray = ->
      list = []
      for key in ordering
        list.push buckets[key]
      return list

    service.getTop = ->
      index = ordering.length - 1
      if index < 0 then return null
      return buckets[ordering[index]]

    service.getBucketItemByName = (name) ->
      return buckets[name]

    return service

  module.factory '$cnBucketProvider', bucketProvider


  #############################################################################
  ## BucketService
  #############################################################################


  class BucketService extends carneades.Service
    @.$inject = [
      '$cnBucketProvider'
      '$state'
      '$stateParams'
      '$translate'
    ]

    constructor: (@cnBucketProvider, @state, @stateParams, @translate) ->

      @.visitedLast = null

    _build: ->
      _translate = (stateName) =>
        label = @translate.instant @state.get(stateName).label
        return label

      return {
        label: _translate @state.$current.name
        name: @state.$current.name
        params: angular.copy @stateParams
        tooltip: @state.$current.tooltip
      }

    getLastVisitedBucket: -> return @.visitedLast

    setLastVisitedBucket: (state) -> @.visitedLast = state

    getBucketItemByName: (name) ->
      return @cnBucketProvider.getBucketItemByName name

    isEmpty: -> return @cnBucketProvider.isEmpty()

    size: -> return @cnBucketProvider.size()

    append: (state) ->
      @cnBucketProvider.add @._build state

    peek: ->
      return @cnBucketProvider.getTop()

    remove: (state) ->
      @cnBucketProvider.remove state

    getBucketItems: ->
      return @cnBucketProvider.asArray()

    getRenderedBucketItems: ->
      items = @cnBucketProvider.asArray()

      _render = (states) =>
        _fnSetIsActive = (s) =>
          s.isActive = (@state.$current.name is s.name)
          return s

        return (_fnSetIsActive(s) for s in states)

      return _render items

  module.service '$cnBucket', BucketService


  class BucketnavController extends carneades.Controller
    @.$inject = [
      '$scope'
      '$state'
    ]

    constructor: (@scope, @state) ->
      super()

  module.controller 'BucketnavController', BucketnavController


  bucketnavDirective = ->
    restrict: 'E'
    scope:
      states: '='
    replace: true
    templateUrl: 'bucketnav/bucketnav.jade'
    controller: 'BucketnavController'


  module.directive 'bucketnav', bucketnavDirective


  bucketnavContainerDirective = () ->
    restrict: 'EA'
    scope:
      states: '='
    replace: true
    transclude: true
    templateUrl: 'bucketnav/bucketnav-container.jade'


  module.directive 'bucketnavContainer', bucketnavContainerDirective


  class BucketnavEntryController extends carneades.Controller
    @.$inject = [
      '$scope'
      '$state'
    ]

    constructor: (@scope, @state) ->
      super()

    open: (name, params) ->
      @state.go name, params


  module.controller 'BucketnavEntryController', BucketnavEntryController


  bucketnavEntryDirective = ->
    restrict: 'E'
    replace: true
    scope:
      state: '='
      index: '='
    templateUrl: 'bucketnav/bucketnav-entry.jade'
    controller: 'BucketnavEntryController'
    link: (scope, elm, ctrl) ->
      scope.open = -> (a, b) ->
        console.log 'found'
      if scope.state.isActive
        elm.find('a').find('button').addClass "active"
      else elm.find('a').find('button').removeClass "active"

  module.directive 'bucketnavEntry', bucketnavEntryDirective
