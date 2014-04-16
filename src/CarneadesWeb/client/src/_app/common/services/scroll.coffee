# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular'], (angular) ->
  angular.module('services.scroll', []).service 'scroll', ($location, $anchorScroll) ->
    scrollTo: (id) ->
      setTimeout ->
        window.scrollTo(window.pageXOffset, window.pageYOffset - 90)

        el = angular.element (document.getElementById id)
        el.addClass 'scrolled'
        old = $location.hash()
        $location.hash id
        $anchorScroll()
        $location.hash old
      , 200
    
