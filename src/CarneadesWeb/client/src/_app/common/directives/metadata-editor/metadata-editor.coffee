# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

# Display an image representing the value of the evaluation
define [
  'angular'
], (angular) ->
  angular.module("directives.metadataEditor", [])

  .directive("metadataEditor", ->
    templateUrl: "common/directives/metadata-editor/metadata-editor.jade"
    restrict: "E"
    scope:
      value: '='
  )
