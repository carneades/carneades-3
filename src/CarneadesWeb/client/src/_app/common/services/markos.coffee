# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular'], (angular) ->
  angular.module('services.markos', []).service 'markos', ["$http", "urlService", ($http, urlService) ->
    class Markos
        
      setUserId: (id) ->
        @userId = id
        
      getUserId: () ->
        @userId

      setEntityUri: (uri) ->
        @entityUri = uri

      getEntityUri: (uri) ->
        @entityUri

      share: (url) ->
        console.log "share on markos NYI, userId=#{@userId} entityUri=#{@entityUri} url=#{url}"
        resource = urlService.$resource "/lican/analyses"
        resource.save({}, {entityuri: @entityUri, url: url})
          .$promise.then((a) ->
            console.log "successfully sent analysis to the triplestore"
            window.alert "Thanks for sharing."
            undefined
          ,
          (reason) ->
            window.alert reason.statusText
            undefined
        )

    new Markos
    ]
