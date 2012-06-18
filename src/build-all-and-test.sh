#!/usr/bin/env bash

# build one standalone and one toolbox WAR and run tests
sh -c "./build.sh --release" && \
    cd PolicyModellingTool && \
    mv policymodellingtool-1.0.0-SNAPSHOT-standalone.war policymodellingtool-1.0.0-SNAPSHOT-toolbox.war && \
    sh -c "./scripts/make-dev.sh" && \
    lein ring uberwar && \
    cd ../CarneadesEngine && lein test && \
    cd ../CarneadesWebService && \
    lein test

