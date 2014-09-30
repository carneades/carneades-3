# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular'], (angular) ->
  angular.module('services.legalprofileInfo', []).service 'legalprofileInfo',
  () ->
    _getTheoryIds = (section) ->
      ids = {}
      
      for id in (scheme.id for scheme in section.schemes)
        ids[id] = id
        
      for subsection in section.sections
        subIds = _getTheoryIds subsection
        for id in subIds
          ids[id] = id

      (val for k,val of ids)
    
    isSchemeOut: (legalprofile, scheme) ->
      _isOut = (r for r in legalprofile.rules when r.ruleid == scheme.id and r.value == 0.0).length > 0

    isSchemeIn: (legalprofile, scheme) ->
      _isOut = (r for r in legalprofile.rules when r.ruleid == scheme.id and r.value == 1.0).length > 0
        
    isSchemeUndecided: (legalprofile, scheme) ->
      _isOut = ((r for r in legalprofile.rules when r.ruleid == scheme.id and r.value == 0.5).length > 0) or ((r for r in legalprofile.rules when r.ruleid == scheme.id).length == 0)

    # Returns all the ids of all the schemes contained in the theory
    getTheoryIds: (section) ->
      _getTheoryIds section
