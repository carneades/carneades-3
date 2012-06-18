#!/usr/bin/env bash

# build one standalone and one toolbox WAR
sh ./build.sh --release
cd PolicyModellingTool
mv policymodellingtool-1.0.0-SNAPSHOT-standalone.war policymodellingtool-1.0.0-SNAPSHOT-toolbox.war

./scripts/make-dev.sh

# run tests

cd ../CarneadesEngine && lein test && cd ../CarneadesWebService && lein test
