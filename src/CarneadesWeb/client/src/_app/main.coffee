# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global require, document
require.config
  shim:
    "angular":
      deps: ['jquery', 'jquery-ui', 'jquery-htmlclean', 'rangy-core', 'hallo']
      exports: "angular"

    "angular-ui-router":
      deps: ["angular"]

    "angular-resource":
      deps: ["angular"]

    "angular-markdown":
      deps: ["angular", "showdown"]

    "angular-translate":
      deps: ["angular"]

    "angular-capitalize-filter":
      deps: ["angular"]

    "angular-translate-loader-static-files":
      deps: ["angular-translate"]

    "angular-ui-bootstrap":
      deps: ["angular"]

    "jquery":
      exports: "$q"

    "jquery-ui":
      deps: ['jquery']

    "jquery-htmlclean":
      deps: ['jquery']

    "hallo":
      deps: ["jquery", "jquery-ui", "jquery-htmlclean", "rangy-core"]

    "perfect-scrollbar":
      deps: ["jquery", "jquery-mousewheel"]

    "jquery-mousewheel":
      deps: ["jquery"]

  paths:
    "angular": "./libs/angular"
    'angular-perfect-scrollbar': './libs/angular-perfect-scrollbar/angular-perfect-scrollbar'
    'angular-sanitize': './libs/angular-sanitize'
    'angular-ui-router': './libs/angular-ui-router'
    'angular-ui-utils': './libs/angular-ui-utils'
    'angular-resource': './libs/angular-resource'
    'angular-markdown': './libs/angular-markdown'
    'angular-translate': './libs/angular-translate'
    'angular-capitalize-filter': './libs/angular-capitalize-filter'
    'angular-translate-loader-static-files': './libs/angular-translate-loader-static-files'
    'angular-bootstrap': './libs/angular-bootstrap'
    'jquery-mousewheel': './libs/angular-perfect-scrollbar/jquery.mousewheel'
    'perfect-scrollbar': './libs/angular-perfect-scrollbar/perfect-scrollbar'
    'requirejs-domready': './libs/requirejs-domready'
    'showdown': './libs/showdown/showdown'
    'spinjs': './libs/spin'
    'jquery': './libs/jquery'
    'jquery-ui': './libs/jquery-ui'
    'jquery-htmlclean': './libs/jquery-htmlclean'
    'rangy-core': './libs/rangy-core'
    'hallo': './libs/hallo'

  deps: ["./bootstrap"]
