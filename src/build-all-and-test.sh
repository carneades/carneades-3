#!/usr/bin/env bash

# build one standalone and one toolbox WAR and run tests
bash -c "./build.sh --release" && \
    cd PolicyModellingTool && \
    mv target/policymodellingtool-1.0.0-SNAPSHOT-standalone.war target/policymodellingtool-1.0.0-SNAPSHOT-toolbox.war && \
    lein with-profile war ring uberwar && \
    mv target/policymodellingtool-1.0.0-SNAPSHOT-standalone.war target/policymodellingtool-1.0.0-SNAPSHOT-css.war
    cd ../CarneadesEngine && lein test && \
    cd ../CarneadesWebService && \
    lein test

