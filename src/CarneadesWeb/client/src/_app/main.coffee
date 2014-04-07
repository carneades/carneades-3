# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

#global require, document
require.config
  shim:
    angular:
      exports: "angular"

    "angular-ui-router":
      deps: ["angular"]

    "angular-resource":
      deps: ["angular"]

    "showdown-carneades":
      deps: ["components/showdown/compressed/showdown"]

    "angular-markdown":
      deps: ["angular", "components/showdown/compressed/showdown"]

    "angular-translate":
      deps: ["angular"]

    "angular-translate-loader-static-files":
      deps: ["angular-translate"]

    "angular-ui-bootstrap3-patched":
      deps: ["angular"]

  deps: ["./bootstrap"]
