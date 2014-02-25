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

    "angular-ui-bootstrap3-patched":
      deps: ["angular"]

  deps: ["./bootstrap"]
